import type { Tagged } from 'type-fest';
import { execCli, type CliArgs, type CommandError, type Path } from './exec.js';
import * as fs from 'node:fs'
import * as json from '@konduit/codec/json';
import { err, ok, type Result } from 'neverthrow';
import { Json } from '@konduit/codec/json';
import { Contract } from '@marlowe-lang/language/v1';
import { ContractId, ContractState, PostCreateContractResponse, ApplyInputsResponse, ContractSourceId, PostContractSourceResponse, Next } from '@marlowe-lang/runtime/client';
import type { JsonError } from '@konduit/codec/json/codecs';
import type { NormalInput } from '@marlowe-lang/language/v1';
import { tmpdir } from 'node:os';
import * as path from 'node:path';
import type { AddressBech32, NetworkMagicNumber } from '@konduit/konduit-consumer/cardano';

export type MarloweRuntimeConfig = {
  readonly serverHost?: string;
  readonly serverPort?: number;
  readonly testnetMagic: NetworkMagicNumber;
  readonly socketPath: string;
};

export function execMarloweRuntimeClient(
  args: CliArgs,
  config: MarloweRuntimeConfig,
  repoRoot: Path | null = null,
  debug: boolean = false,
): Result<string, CommandError> {
  const marloweRuntimeCli = (() => {
    if (process.env.MARLOWE_RUNTIME_CLIENT) {
      return process.env.MARLOWE_RUNTIME_CLIENT;
    }
    repoRoot = repoRoot ?? process.env.ROOT_DIR as Path | null ?? process.cwd() as Path;
    return `cabal run -v0 --project-file ${repoRoot}/cabal.project marlowe-runtime:cli --`;
  })();

  const opts = [...args] as CliArgs;

  if (config.serverPort !== undefined) {
    opts.push(['--server-port', String(config.serverPort)]);
  }
  if (config.serverHost !== undefined) {
    opts.push(['--server-host', config.serverHost]);
  }
  const needsSocketPath = args.length > 1 && args[0] === 'contract' && (args[1] === 'init' || args[1] === 'apply-inputs');
  if (needsSocketPath) {
    opts.push(['--socket-path', config.socketPath]);
  }
  return execCli(marloweRuntimeCli, opts, debug);
}

export function execMarloweRuntimeClientJson(
  args: CliArgs,
  config: MarloweRuntimeConfig,
  repoRoot: Path | null = null,
  debug: boolean = false,
): Result<Json, CommandError | JsonError> {
  return execMarloweRuntimeClient(args, config, repoRoot, debug).andThen(jsonStr => Json.fromString(jsonStr));
}

export function execMarloweRuntimeClientJsonTyped<T>(
  args: CliArgs,
  deserialiser: (json: Json) => Result<T, JsonError>,
  config: MarloweRuntimeConfig,
  repoRoot: Path | null = null,
  debug: boolean = false,
): Result<T, CommandError | JsonError> {
  return execMarloweRuntimeClientJson(args, config, repoRoot, debug).andThen(deserialiser);
}

export function writeContractFile(contractFile: Path, contract: Contract): MarloweContractFile {
  fs.writeFileSync(contractFile, json.stringify(contract as Json, undefined, 2))
  return contractFile as MarloweContractFile;
}

export type MarloweContractFile = Tagged<Path, 'MarloweContractFile'>;

const mkTempDir = (local: boolean = false): string => {
  const rootDir = local ? process.cwd() : tmpdir();
  const prefix = path.join(rootDir, 'marlowe-runtime-client-temp-');
  return fs.mkdtempSync(prefix);
}

// $ cabal run marlowe-runtime-client -- contract init --help
// Usage: marlowe-runtime-client contract init
//          --contract-file CONTRACT_FILE [--devel-scripts]
//          --funding-wallet-address BECH32 [--message-format text|json|yaml]
//          [--mainnet | --testnet-magic INTEGER] [-s|--socket-path ARG]
//          [-o|--output-dir ARG]
//          [-h|--server-host HOST] [-p|--server-port PORT]
export function runInitCLI(
  initRef: { kind: 'file', contractFile: MarloweContractFile } | { kind: 'source', contractSourceId: ContractSourceId },
  fundingWalletAddress: AddressBech32,
  config: MarloweRuntimeConfig,
  options: {
    develScripts?: boolean;
    outputDir?: string;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'contract', 'init',
    ['--message-format', 'json'],
    ['--funding-wallet-address', fundingWalletAddress],
  ];
  switch (initRef.kind) {
    case 'file':
      args.push(['--contract-file', initRef.contractFile]);
      break;
    case 'source':
      args.push(['--contract-source-id', initRef.contractSourceId]);
      break;
  }
  if (options.develScripts) {
    args.push('--devel-scripts');
  }
  if (options.outputDir) {
    args.push(['--output-dir', options.outputDir]);
  }
  return execMarloweRuntimeClient(args, config, repoRoot, debug)
    .andThen(jsonStr => Json.fromString(jsonStr));
}

export function runInit(
  contract: Contract,
  fundingWalletAddress: AddressBech32,
  config: MarloweRuntimeConfig,
  options: {
    develScripts?: boolean;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<PostCreateContractResponse, JsonError | CommandError | string> {
  const tmpDir = mkTempDir(false);
  const contractFile = writeContractFile(`${tmpDir}/contract.json` as Path, contract);
  return runInitCLI({ kind: 'file', contractFile }, fundingWalletAddress, config, { ...options, outputDir: tmpDir }, repoRoot, debug)
    .andThen(json => PostCreateContractResponse.jsonCodec.deserialise(json));
}

// Deploy a contract from a previously uploaded contract source by id. The
// runtime resolves the source from its store instead of taking the full
// contract inline. The returned envelope is signed and ready to submit.
export function runInitBySource(
  sourceId: ContractSourceId,
  fundingWalletAddress: AddressBech32,
  config: MarloweRuntimeConfig,
  options: {
    develScripts?: boolean;
    outputDir?: string;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<PostCreateContractResponse, JsonError | CommandError | string> {
  return runInitCLI(
    { kind: 'source', contractSourceId: sourceId },
    fundingWalletAddress,
    config,
    options,
    repoRoot,
    debug,
  ).andThen(json => PostCreateContractResponse.jsonCodec.deserialise(json));
}

// Usage: cli contract get --contract-id CONTRACT_ID
//                         [--message-format text|json|yaml]
//                         [-h|--server-host HOST] [-p|--server-port PORT]
export const runGetCLI = (
  contractId: ContractId,
  config: MarloweRuntimeConfig,
  options: { expand?: boolean } = {},
  repoRoot: Path | null = null,
  debug: boolean = false,
): Result<Json, CommandError | string> => {
  const args: CliArgs = [
    'contract', 'get',
    ['--contract-id', contractId],
    ['--message-format', 'json'],
  ];
  if (options.expand) {
    args.push('--expand');
  }
  return execMarloweRuntimeClient(args, config, repoRoot, debug).andThen(jsonStr => {
    return Json.fromString(jsonStr).match(
      (json) => ok(json),
      (e) => err(e),
    );
  });
};

export function runGet(
  contractId: ContractId,
  config: MarloweRuntimeConfig,
  options: { expand?: boolean } = {},
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ContractState, JsonError | CommandError | string> {
  return runGetCLI(contractId, config, options, repoRoot, debug)
    .andThen(json => ContractState.jsonCodec.deserialise(json));
}

export function runApplyInputsCLI(
  marloweInputsFile: Path,
  contractId: ContractId,
  userWalletAddress: AddressBech32,
  config: MarloweRuntimeConfig,
  options: { outputDir?: string } = {},
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'contract', 'apply-inputs',
    ['--marlowe-inputs-file', marloweInputsFile],
    ['--contract-id', contractId],
    ['--user-wallet-address', userWalletAddress],
    ['--message-format', 'json'],
  ];
  if (options.outputDir) {
    args.push(['--output-dir', options.outputDir]);
  }
  return execMarloweRuntimeClient(args, config, repoRoot, debug)
    .andThen(jsonStr => Json.fromString(jsonStr));
}

export function runApplyInputs(
  marloweInputs: NormalInput[],
  contractId: ContractId,
  userWalletAddress: AddressBech32,
  config: MarloweRuntimeConfig,
  options: { outputDir?: string } = {},
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ApplyInputsResponse, JsonError | CommandError | string> {
  const tmpDir = options.outputDir ?? mkTempDir(false);
  const finalOptions = { ...options, outputDir: tmpDir };
  const marloweInputsFile = `${tmpDir}/marlowe-inputs.json` as Path;
  fs.writeFileSync(marloweInputsFile, json.stringify(marloweInputs as Json, undefined, 2));
  return runApplyInputsCLI(marloweInputsFile, contractId, userWalletAddress, config, finalOptions, repoRoot, debug)
    .andThen(json => ApplyInputsResponse.jsonCodec.deserialise(json));
}

// Usage: cli contract next --contract-id CONTRACT_ID
//                      --validity-start ISO_TIMESTAMP
//                      --validity-end ISO_TIMESTAMP [--party ROLE_OR_ADDRESS]...
//                      [--message-format text|json|yaml]
//                      [-h|--server-host HOST] [-p|--server-port PORT]

// | FIXME: This is default is dangerous and misleading.
// | It should be derived from the contract itself.
// | We should look through the cases and timeout branch
// | to find out possible validity windows.
const NEXT_VALIDITY_WINDOW_MS = 5 * 60 * 1000;

export function runNext(
  contractId: ContractId,
  parties: AddressBech32[] | undefined,
  config: MarloweRuntimeConfig,
  options: {
    validityStart?: string;
    validityEnd?: string;
  } = {},
  repoRoot: Path | null = null,
  debug: boolean = false,
): Result<Next, JsonError | CommandError | string> {
  const now = new Date();
  const end = new Date(now.getTime() + NEXT_VALIDITY_WINDOW_MS);
  const validityStart = options.validityStart ?? now.toISOString();
  const validityEnd = options.validityEnd ?? end.toISOString();
  const args: CliArgs = [
    'contract', 'next',
    ['--contract-id', contractId],
    ['--validity-start', validityStart],
    ['--validity-end', validityEnd],
    ['--message-format', 'json'],
  ];

  if (parties && parties.length > 0) {
    for (const party of parties) {
      args.push(['--party', party]);
    }
  }

  return execMarloweRuntimeClientJsonTyped(args, Next.jsonCodec.deserialise, config, repoRoot, debug);
}

// Usage: cli store upload --bundle-file BUNDLE_FILE --main LABEL
//                          [--message-format text|json|yaml]
//                          [--preserve-actions JSON]
//                          [-h|--server-host HOST] [-p|--server-port PORT]
export function runUploadContractSource(
  bundle: unknown[],
  mainLabel: string,
  config: MarloweRuntimeConfig,
  options: { preserveActions?: unknown[] } = {},
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<PostContractSourceResponse, JsonError | CommandError | string> {
  const tmpDir = mkTempDir(false);
  const bundleFile = `${tmpDir}/bundle.json` as Path;
  fs.writeFileSync(bundleFile, json.stringify(bundle as Json, undefined, 2));
  const args: CliArgs = [
    'store', 'upload',
    ['--bundle-file', bundleFile],
    ['--main', mainLabel],
    ['--message-format', 'json'],
  ];
  if (options.preserveActions !== undefined) {
    args.push(['--preserve-actions', json.stringify(options.preserveActions as Json)]);
  }
  return execMarloweRuntimeClientJsonTyped(args, PostContractSourceResponse.jsonCodec.deserialise, config, repoRoot, debug);
}

export function runGetContractSourceCLI(
  contractSourceId: ContractSourceId,
  config: MarloweRuntimeConfig,
  options: { expand?: boolean } = {},
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'store', 'get',
    ['--contract-source-id', contractSourceId],
    ['--message-format', 'json'],
  ];
  if (options.expand) {
    args.push('--expand');
  }
  return execMarloweRuntimeClient(args, config, repoRoot, debug).andThen(jsonStr =>
    Json.fromString(jsonStr),
  );
}

export function runGetContractSource(
  contractSourceId: ContractSourceId,
  config: MarloweRuntimeConfig,
  options: { expand?: boolean } = {},
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, JsonError | CommandError | string> {
  // FIXME: paluh: deserialize into a `Contract` once a JSON codec is exposed
  // by the runtime client.
  return runGetContractSourceCLI(contractSourceId, config, options, repoRoot, debug);
}

// Usage: cli store adjacency --contract-source-id CONTRACT_SOURCE_ID
//                              [--message-format text|json|yaml]
export function runGetContractSourceAdjacencyCLI(
  contractSourceId: ContractSourceId,
  config: MarloweRuntimeConfig,
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'store', 'adjacency',
    ['--contract-source-id', contractSourceId],
    ['--message-format', 'json'],
  ];
  return execMarloweRuntimeClient(args, config, repoRoot, debug).andThen(jsonStr =>
    Json.fromString(jsonStr),
  );
}

export function runGetContractSourceAdjacency(
  contractSourceId: ContractSourceId,
  config: MarloweRuntimeConfig,
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ContractSourceId[], JsonError | CommandError | string> {
  return runGetContractSourceAdjacencyCLI(contractSourceId, config, repoRoot, debug)
    .andThen(json => deserialiseArrayOfContractSourceIds(json));
}

// Usage: cli store closure --contract-source-id CONTRACT_SOURCE_ID
//                            [--message-format text|json|yaml]
export function runGetContractSourceClosureCLI(
  contractSourceId: ContractSourceId,
  config: MarloweRuntimeConfig,
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'store', 'closure',
    ['--contract-source-id', contractSourceId],
    ['--message-format', 'json'],
  ];
  return execMarloweRuntimeClient(args, config, repoRoot, debug).andThen(jsonStr =>
    Json.fromString(jsonStr),
  );
}

export function runGetContractSourceClosure(
  contractSourceId: ContractSourceId,
  config: MarloweRuntimeConfig,
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ContractSourceId[], JsonError | CommandError | string> {
  return runGetContractSourceClosureCLI(contractSourceId, config, repoRoot, debug)
    .andThen(json => deserialiseArrayOfContractSourceIds(json));
}

// FIXME: paluh: this is a stopgap while the runtime client lacks a JSON
// codec for `ContractSourceId[]`. Once added, replace with the codec.
function deserialiseArrayOfContractSourceIds(
  json: Json,
): Result<ContractSourceId[], JsonError> {
  if (!Array.isArray(json)) {
    return err(`Expected a JSON array of contract source ids, got: ${JSON.stringify(json)}` as unknown as JsonError);
  }
  const ids: ContractSourceId[] = [];
  for (const [idx, entry] of json.entries()) {
    const decoded = ContractSourceId.jsonCodec.deserialise(entry);
    if (decoded.isErr()) {
      return err(`Invalid contract source id at index ${idx}: ${JSON.stringify(entry)}` as unknown as JsonError);
    }
    ids.push(decoded.value);
  }
  return ok(ids);
}
