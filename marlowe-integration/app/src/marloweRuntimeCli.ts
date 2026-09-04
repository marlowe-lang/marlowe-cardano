import type { Tagged } from 'type-fest';
import { execCli, type CliArgs, type CommandError, type Path } from './exec.js';
import * as fs from 'node:fs'
import type { PositiveInt } from '@konduit/codec/integers/smallish';
import * as json from '@konduit/codec/json';
import { err, ok, type Result } from 'neverthrow';
import { Json } from '@konduit/codec/json';
import type { Contract } from '@marlowe-lang/language/v1';
import { ContractId, ContractState, PostCreateContractResponse, ApplyInputsResponse, ContractSourceId, PostContractSourceResponse, Next, CanDeposit, CanChoose, CanNotify } from '@marlowe-lang/runtime/client';
import type { JsonError } from '@konduit/codec/json/codecs';
import type { NormalInput } from '@marlowe-lang/language/v1';
import { tmpdir } from 'node:os';
import * as path from 'node:path';
import type { AddressBech32, NetworkMagicNumber } from '@konduit/konduit-consumer/cardano';

function getMarloweRuntimeClient(repoRoot: Path | null = null): string {
  if (process.env.MARLOWE_RUNTIME_CLIENT) {
    return process.env.MARLOWE_RUNTIME_CLIENT;
  }
  repoRoot = repoRoot ?? process.env.ROOT_DIR as Path | null ?? process.cwd() as Path;
  return `cabal run -v0 --project-file ${repoRoot}/cabal.project marlowe-runtime:cli --`;
}

export function execMarloweRuntimeClient(args: CliArgs, repoRoot: Path | null = null, debug: boolean = false): Result<string, CommandError> {
  const marloweCli = getMarloweRuntimeClient(repoRoot);
  return execCli(marloweCli, args, debug);
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
// 
//   Build initial marlowe transaction
// 
// Available options:
//   --contract-file CONTRACT_FILE
//                            JSON input file for the contract.
//   --devel-scripts          Compile the devel script variants with tracing
//                            preserved.
//   --funding-wallet-address BECH32
//                            The address which pays for claim transactions.
//   --message-format text|json|yaml
//                            Format of command output. (default: text)
//   --mainnet                Execute on mainnet.
//   --testnet-magic INTEGER  Network magic. Defaults to the CARDANO_TESTNET_MAGIC
//                            environment variable's value.
//   -s,--socket-path ARG     Location of the cardano-node socket file. Defaults to
//                            the CARDANO_NODE_SOCKET_PATH environment variable's
//                            value.
//                            (default: "/home/paluh/projects/marlowe/marlowe-plutus/.run/testnet/state-cluster9/bft1.socket")
//   -o,--output-dir ARG      Directory where transaction file will be written.
//                            (default: "out")
//   -h,--help                Show this help text
export function runInitCLI(
  initRef: { kind: 'file', contractFile: MarloweContractFile } | { kind: 'source', contractSourceId: ContractSourceId },
  fundingWalletAddress: AddressBech32,
  options: {
    develScripts?: boolean;
    outputDir?: string;
    serverPort?: PositiveInt;
    socketPath?: string;
    testnetMagic?: NetworkMagicNumber,
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
  if (options.testnetMagic) {
    args.push(['--testnet-magic', options.testnetMagic]);
  }
  if (options.socketPath) {
    args.push(['--socket-path', options.socketPath]);
  }
  if (options.outputDir) {
    args.push(['--output-dir', options.outputDir]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr => Json.fromString(jsonStr));
}

export function runInit(
  contract: Contract,
  fundingWalletAddress: AddressBech32,
  options: {
    develScripts?: boolean;
    serverPort?: PositiveInt;
    socketPath?: string;
    testnetMagic?: NetworkMagicNumber,
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<PostCreateContractResponse, JsonError | CommandError | string> {
  const tmpDir = mkTempDir(false);
  const contractFile = writeContractFile(`${tmpDir}/contract.json` as Path, contract);
  const finalOptions = {
    outputDir: tmpDir,
    ...options,
  }
  return runInitCLI({ kind: 'file', contractFile }, fundingWalletAddress, finalOptions, repoRoot, debug)
    .andThen(json => PostCreateContractResponse.jsonCodec.deserialise(json));
}

// Deploy a contract from a previously uploaded contract source by id. The
// runtime resolves the source from its store instead of taking the full
// contract inline. The returned envelope is signed and ready to submit.
export function runInitBySource(
  sourceId: ContractSourceId,
  fundingWalletAddress: AddressBech32,
  options: {
    develScripts?: boolean;
    outputDir?: string;
    serverPort?: PositiveInt;
    socketPath?: string;
    testnetMagic?: NetworkMagicNumber,
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<PostCreateContractResponse, JsonError | CommandError | string> {
  const finalOptions = {
    outputDir: options.outputDir ?? mkTempDir(false),
    serverPort: options.serverPort,
    socketPath: options.socketPath,
    testnetMagic: options.testnetMagic,
    develScripts: options.develScripts,
  };
  return runInitCLI(
    { kind: 'source', contractSourceId: sourceId },
    fundingWalletAddress,
    finalOptions,
    repoRoot,
    debug,
  ).andThen(json => PostCreateContractResponse.jsonCodec.deserialise(json));
}
// Usage: cli contract get --contract-id CONTRACT_ID
//                         [--message-format text|json|yaml]
//                         [-h|--server-host HOST] [-p|--server-port PORT]
// 
//   Get the current state of a Marlowe contract from the web server.
// 
// Available options:
//   --contract-id CONTRACT_ID
//                            The id of the Marlowe contract to query, in the
//                            format <txId>#<txIx>.
//   --message-format text|json|yaml
//                            Format of command output. (default: text)
//   -h,--server-host HOST    The host on which the web server is running. Defaults
//                            to localhost. (default: Host "localhost")
//   -p,--server-port PORT    The port on which the web server is running. Defaults
//                            to 8090.
//   -h,--help                Show this help text
//
export const runGetCLI = (
  contractId: ContractId,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> => {
  const args: CliArgs = [
    'contract', 'get',
    ['--contract-id', contractId],
    ['--message-format', 'json'],
  ];
  if (options.serverHost) {
    args.push(['--server-host', options.serverHost]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr => {
    return Json.fromString(jsonStr).match(
      (json) => ok(json),
      (e) => {
        console.error("Failed to parse JSON output from marlowe-runtime-client:");
        console.error(e);
        console.error("Original output:");
        console.error(jsonStr);
        return err(e);
      }
    );
  });
}

export function runGet(
  contractId: ContractId,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ContractState, JsonError | CommandError | string> {
  return runGetCLI(contractId, options, repoRoot, debug).andThen(json => ContractState.jsonCodec.deserialise(json));
}

// Usage: cli contract apply-inputs (-i|--marlowe-inputs-file MARLOWE_INPUTS_FILE)
//                                  --contract-id CONTRACT_ID
//                                  [--message-format text|json|yaml]
//                                  [--mainnet | --testnet-magic INTEGER]
//                                  [-s|--socket-path ARG] [-o|--output-dir ARG]
//                                  [-h|--server-host HOST] [-p|--server-port PORT]
//                                  (-w|--user-wallet-address BECH32)
// 
//   Build input application transaction
// 
// Available options:
//   -i,--marlowe-inputs-file MARLOWE_INPUTS_FILE
//                            JSON file which contains a list of Marlowe contract
//                            inputs which should be applied.
//   --contract-id CONTRACT_ID
//                            The id of the Marlowe contract to query, in the
//                            format <txId>#<txIx>.
//   --message-format text|json|yaml
//                            Format of command output. (default: text)
//   --mainnet                Execute on mainnet.
//   --testnet-magic INTEGER  Network magic. Defaults to the
//                            CARDANO_NODE_NETWORK_ID environment variable's value.
//   -s,--socket-path ARG     Location of the cardano-node socket file. Defaults to
//                            the CARDANO_NODE_SOCKET_PATH environment variable's
//                            value.
//                            (default: "/home/paluh/projects/marlowe/marlowe-plutus/.run/testnet/state-cluster9/bft1.socket")
//   -o,--output-dir ARG      Directory where transaction file will be written.
//                            (default: "out")
//   -h,--server-host HOST    The host on which the web server is running. Defaults
//                            to localhost. (default: Host "localhost")
//   -p,--server-port PORT    The port on which the web server is running. Defaults
//                            to 8090.
//   -w,--user-wallet-address BECH32
//                            The address which both pays for the transaction and
//                            executes the contract action.
//   -h,--help                Show this help text
export function runApplyInputsCLI(
  marloweInputsFile: Path,
  contractId: ContractId,
  userWalletAddress: AddressBech32,
  options: {
    outputDir?: string;
    serverHost?: string;
    serverPort?: PositiveInt;
    socketPath?: string;
    testnetMagic?: NetworkMagicNumber;
  },
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
  if (options.serverHost) {
    args.push(['--server-host', options.serverHost]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  if (options.socketPath) {
    args.push(['--socket-path', options.socketPath]);
  }
  if (options.testnetMagic) {
    args.push(['--testnet-magic', options.testnetMagic]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr => Json.fromString(jsonStr));
}

export function runApplyInputs(
  marloweInputs: NormalInput[],
  contractId: ContractId,
  userWalletAddress: AddressBech32,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
    socketPath?: string;
    testnetMagic?: NetworkMagicNumber;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ApplyInputsResponse, JsonError | CommandError | string> {
  const tmpDir = mkTempDir(false);
  const marloweInputsFile = `${tmpDir}/marlowe-inputs.json` as Path;
  fs.writeFileSync(marloweInputsFile, json.stringify(marloweInputs as Json, undefined, 2));
  const finalOptions = {
    outputDir: tmpDir,
    ...options,
  }
  return runApplyInputsCLI(marloweInputsFile, contractId, userWalletAddress, finalOptions, repoRoot, debug)
    .andThen(json => {
      console.debug(json);
      return ApplyInputsResponse.jsonCodec.deserialise(json);
    });
}

// Usage: cli contract next --contract-id CONTRACT_ID
//                      --validity-start ISO_TIMESTAMP
//                      --validity-end ISO_TIMESTAMP [--party ROLE_OR_ADDRESS]...
//                      [--message-format text|json|yaml]
//                      [-h|--server-host HOST] [-p|--server-port PORT]
export function runNextCLI(
  contractId: ContractId,
  validityStart: string,
  validityEnd: string,
  parties: AddressBech32[] | undefined,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false,
): Result<Json, CommandError | string> {
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
  if (options.serverHost) {
    args.push(['--server-host', options.serverHost]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr => Json.fromString(jsonStr));
}

// | Default validity window for `/next` polls: from now to now+5min.
// | Tight enough to keep the validity range under the contract timeout, so
// | we're asserting the runtime finds inputs before the contract expires.
const NEXT_VALIDITY_WINDOW_MS = 5 * 60 * 1000;
const NEXT_POLL_INTERVAL_MS = 2_000;
const NEXT_POLL_TIMEOUT_MS = 120_000;

export function runNext(
  contractId: ContractId,
  parties: AddressBech32[] | undefined,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
    validityStart?: string;
    validityEnd?: string;
  },
  repoRoot: Path | null = null,
  debug: boolean = false,
): Result<Next, JsonError | CommandError | string> {
  const now = new Date();
  const end = new Date(now.getTime() + NEXT_VALIDITY_WINDOW_MS);
  const validityStart = options.validityStart ?? now.toISOString();
  const validityEnd = options.validityEnd ?? end.toISOString();
  return runNextCLI(contractId, validityStart, validityEnd, parties, options, repoRoot, debug)
    .andThen(json => Next.jsonCodec.deserialise(json));
}

// Usage: cli store upload --bundle-file BUNDLE_FILE --main LABEL
//                          [--message-format text|json|yaml]
//                          [-h|--server-host HOST] [-p|--server-port PORT]
//
//   Upload a bundle of marlowe objects as a contract source.
//
// Available options:
//   --bundle-file BUNDLE_FILE
//                            JSON input file for the object bundle (an array of
//                            labelled objects).
//   --main LABEL             The label of the top-level contract object in the
//                            bundle.
//   --preserve-actions JSON  JSON-encoded array of Action values to preserve
//                            during merkleization (omit to merkleize everything).
//   --message-format text|json|yaml
//                            Format of command output. (default: text)
//   -h,--server-host HOST    The host on which the web server is running. Defaults
//                            to localhost. (default: Host "localhost")
//   -p,--server-port PORT    The port on which the web server is running. Defaults
//                            to 8090.
//   -h,--help                Show this help text
export function runUploadContractSourceCLI(
  bundleFile: Path,
  mainLabel: string,
  options: {
    preserveActions?: unknown[];
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'store', 'upload',
    ['--bundle-file', bundleFile],
    ['--main', mainLabel],
    ['--message-format', 'json'],
  ];
  if (options.preserveActions !== undefined) {
    // The server expects a JSON array of Action objects (empty array = no
    // preserved actions). We always emit the header when the caller asks
    // for it explicitly, even when empty, so the server can distinguish
    // "no header sent" from "empty set sent".
    args.push(['--preserve-actions', json.stringify(options.preserveActions as Json)]);
  }
  if (options.serverHost) {
    args.push(['--server-host', options.serverHost]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr =>
    Json.fromString(jsonStr),
  );
}

export function runUploadContractSource(
  bundle: unknown[],
  mainLabel: string,
  options: {
    preserveActions?: unknown[];
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<PostContractSourceResponse, JsonError | CommandError | string> {
  const tmpDir = mkTempDir(false);
  const bundleFile = `${tmpDir}/bundle.json` as Path;
  fs.writeFileSync(bundleFile, json.stringify(bundle as Json, undefined, 2));
  return runUploadContractSourceCLI(bundleFile, mainLabel, options, repoRoot, debug)
    .andThen(json => PostContractSourceResponse.jsonCodec.deserialise(json));
}

// Usage: cli store get --contract-source-id CONTRACT_SOURCE_ID [--expand]
//                       [--message-format text|json|yaml]
//                       [-h|--server-host HOST] [-p|--server-port PORT]
//
//   Get a contract source by ID.
//
// Available options:
//   --contract-source-id CONTRACT_SOURCE_ID
//                            The hex-encoded identifier of the contract source.
//   --expand                 Demerkleize the contract before returning it.
//   --message-format text|json|yaml
//                            Format of command output. (default: text)
//   -h,--server-host HOST    The host on which the web server is running. Defaults
//                            to localhost. (default: Host "localhost")
//   -p,--server-port PORT    The port on which the web server is running. Defaults
//                            to 8090.
//   -h,--help                Show this help text
export function runGetContractSourceCLI(
  contractSourceId: ContractSourceId,
  options: {
    expand?: boolean;
    serverHost?: string;
    serverPort?: PositiveInt;
  },
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
  if (options.serverHost) {
    args.push(['--server-host', options.serverHost]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr =>
    Json.fromString(jsonStr),
  );
}

export function runGetContractSource(
  contractSourceId: ContractSourceId,
  options: {
    expand?: boolean;
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, JsonError | CommandError | string> {
  // FIXME: paluh: deserialize into a `Contract` once a JSON codec is exposed
  // by the runtime client.
  return runGetContractSourceCLI(contractSourceId, options, repoRoot, debug);
}

// Usage: cli store adjacency --contract-source-id CONTRACT_SOURCE_ID
//                              [--message-format text|json|yaml]
//                              [-h|--server-host HOST] [-p|--server-port PORT]
//
//   Get the contract source IDs which are adjacent to the given contract source.
//
// Available options: (same as `store get`)
//   -h,--help                Show this help text
export function runGetContractSourceAdjacencyCLI(
  contractSourceId: ContractSourceId,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'store', 'adjacency',
    ['--contract-source-id', contractSourceId],
    ['--message-format', 'json'],
  ];
  if (options.serverHost) {
    args.push(['--server-host', options.serverHost]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr =>
    Json.fromString(jsonStr),
  );
}

export function runGetContractSourceAdjacency(
  contractSourceId: ContractSourceId,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ContractSourceId[], JsonError | CommandError | string> {
  return runGetContractSourceAdjacencyCLI(contractSourceId, options, repoRoot, debug)
    .andThen(json => deserialiseArrayOfContractSourceIds(json));
}

// Usage: cli store closure --contract-source-id CONTRACT_SOURCE_ID
//                            [--message-format text|json|yaml]
//                            [-h|--server-host HOST] [-p|--server-port PORT]
//
//   Get the contract source IDs which appear in the full hierarchy of the
//   given contract source (including the ID of the source itself).
export function runGetContractSourceClosureCLI(
  contractSourceId: ContractSourceId,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<Json, CommandError | string> {
  const args: CliArgs = [
    'store', 'closure',
    ['--contract-source-id', contractSourceId],
    ['--message-format', 'json'],
  ];
  if (options.serverHost) {
    args.push(['--server-host', options.serverHost]);
  }
  if (options.serverPort) {
    args.push(['--server-port', options.serverPort]);
  }
  return execMarloweRuntimeClient(args, repoRoot, debug).andThen(jsonStr =>
    Json.fromString(jsonStr),
  );
}

export function runGetContractSourceClosure(
  contractSourceId: ContractSourceId,
  options: {
    serverHost?: string;
    serverPort?: PositiveInt;
  },
  repoRoot: Path | null = null,
  debug: boolean = false
): Result<ContractSourceId[], JsonError | CommandError | string> {
  return runGetContractSourceClosureCLI(contractSourceId, options, repoRoot, debug)
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
