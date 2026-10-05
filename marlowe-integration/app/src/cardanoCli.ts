import { type CommandError, type CliArg, type CliArgs, execCli, execCliJsonTyped } from './exec.js';
import * as nodeOs from "node:os";
import * as nodeFs from "node:fs";
import * as path from "node:path";
import {
  type TxHex,
} from "./cardano.js";
import { err, ok, Result, ResultAsync } from 'neverthrow';
import * as codec from '@konduit/codec';
import * as jsonCodecs from '@konduit/codec/json/codecs';
import { type JsonDeserialiser } from '@konduit/codec/json/codecs';
import type { Json } from '@konduit/codec/json';
import type { JsonError } from '@konduit/codec/json/codecs';
import { fromSafePromiseResultFn, unwrapOrPanicWith, toAsync } from '@konduit/konduit-consumer/neverthrow';
import type { Milliseconds } from '@konduit/konduit-consumer/time/duration';
import type { Tagged } from 'type-fest';
import { TxEnvelope } from '@marlowe-lang/runtime/client';
import {
  AddressBech32,
  AssetId,
  AssetName,
  NetworkMagicNumber,
  PositiveCoin,
  ScriptHash,
  TxIdHex,
  TxIx,
  TxOutRefHex,
  Value,
  type TxId,
} from '@konduit/konduit-consumer/cardano';
import { valueRecords2ValueCodec, type ValueRecord } from '@konduit/konduit-consumer/cardano/connectorClient';
import { PositiveBigInt } from '@konduit/codec/integers/big';
import type { Wallet } from './cardano.js';
import type { Path as TaggedPath } from './exec.js';


export const execCardanoCli = (args: CliArgs, debug: boolean = false): Result<string, CommandError> => {
  return execCli('cardano-cli', args, debug);
}

export const execCardanoCliJsonTyped = <T>(args: CliArgs, deserialiser: JsonDeserialiser<T>, debug: boolean = false): Result<T, CommandError | JsonError> => {
  return execCliJsonTyped('cardano-cli', args, deserialiser, debug);
}

export type Path = string;

const mkJson2ValueRecordDeserialiser = (scriptHash: ScriptHash): JsonDeserialiser<ValueRecord[]> => {
  const json2ValueEntriesDeserialiser: JsonDeserialiser<[AssetId, PositiveCoin][]> = codec.pipeDeserialisers(
    jsonCodecs.dictOf(PositiveCoin.jsonCodec).deserialise,
    (innerObj: { [assetNameHex: string]: PositiveCoin }) => {
      const tokens: Result<[AssetId, PositiveCoin], JsonError>[] = Object.entries(innerObj)
        .map(([assetNameHex, positiveCoin]: [string, PositiveCoin]) =>
          AssetName.jsonCodec.deserialise(assetNameHex).map((assetName: AssetName) => {
            const assetId = [scriptHash, assetName];
            return [assetId, positiveCoin] as [AssetId, PositiveCoin]
          })
        );
      return Result.combineWithAllErrors(tokens);
    }
  );
  return codec.rmapDeserialiser(
    json2ValueEntriesDeserialiser,
    (entries: [AssetId, PositiveCoin][]) => entries.map(([unit, quantity]) => ({ unit, quantity } as ValueRecord))
  );
}

const json2ValueRecordsDeserialiser: JsonDeserialiser<ValueRecord[]> = codec.pipeDeserialisers(
  jsonCodecs.json2ObjectCodec.deserialise,
  (obj: { [key: string]: Json }): Result<ValueRecord[], JsonError> => {
    const step = ([unit, quantityOrInnerObj]: [string, Json]): Result<ValueRecord[], JsonError> => {
      if (unit === "lovelace") {
        return PositiveCoin.jsonCodec.deserialise(quantityOrInnerObj).map(quantity => [{unit: "lovelace", quantity}]);
      }
      return ScriptHash.jsonCodec.deserialise(unit).andThen((scriptHash: ScriptHash) =>
        mkJson2ValueRecordDeserialiser(scriptHash)(quantityOrInnerObj)
      );
    };
    const results: Result<ValueRecord[], JsonError>[] = Object.entries(obj).map(step);
    return Result.combineWithAllErrors(results).map(recordsArrays => recordsArrays.flat());
  }
);

const json2ValueDeserialiser: JsonDeserialiser<Value> = codec.pipeDeserialisers(
  json2ValueRecordsDeserialiser,
  valueRecords2ValueCodec.deserialise
);

export type TxOut = {
  address: AddressBech32;
  datum: string | null;
  datumhash: string | null;
  inlineDatum: string | null;
  inlineDatumRaw: string | null;
  referenceScript: string | null;
  value: Value;
};

export namespace TxOut {
  export const jsonDeserialiser: JsonDeserialiser<TxOut> = (() => {
    const optStr = jsonCodecs.nullableDeserialiser(jsonCodecs.json2StringCodec.deserialise);
    return jsonCodecs.objectOfDeserialiser({
      address: codec.pipeDeserialisers(
        jsonCodecs.json2StringCodec.deserialise,
        AddressBech32.fromString
      ),
      datum: optStr,
      datumhash: optStr,
      inlineDatum: optStr,
      inlineDatumRaw: optStr,
      referenceScript: optStr,
      value: json2ValueDeserialiser,
    });
  })();
}

export type UtxoMap = Tagged<Map<TxOutRefHex, TxOut>, 'UtxoMap'>;

export namespace UtxoMap {
  export const jsonDeserialiser: JsonDeserialiser<UtxoMap> = codec.pipeDeserialisers(
    jsonCodecs.dictOfDeserialiser(TxOut.jsonDeserialiser),
    (obj: { [txOutRefHex: string]: TxOut }): Result<UtxoMap, JsonError> => {
      const entries: Result<[TxOutRefHex, TxOut], JsonError>[] = Object.entries(obj)
        .map(([txOutRefHex, txOut]) =>
          TxOutRefHex.fromString(txOutRefHex).map((txOutRef: TxOutRefHex) => [txOutRef, txOut] as [TxOutRefHex, TxOut])
        );
      const result: Result<Map<TxOutRefHex, TxOut>, JsonError> = Result.combineWithAllErrors(entries).map((entriesArray: [TxOutRefHex, TxOut][]) => {
        const utxoMap = new Map<TxOutRefHex, TxOut>();
        for (const [txOutRef, txOut] of entriesArray) {
          utxoMap.set(txOutRef, txOut);
        }
        return utxoMap;
      });
      return result as Result<UtxoMap, JsonError>;
    }
  );
  export const toUtxos = (utxoMap: UtxoMap): Utxo[] => {
    const result: Utxo[] = [];
    utxoMap.forEach((txOut, txOutRef) => {
      result.push({
        txOutRef,
        txOut,
      });
    });
    return result;
  }
}

export type Utxo = { txOutRef: TxOutRefHex; txOut: TxOut };

export type Tip = {
  block: PositiveBigInt,
  epoch: PositiveBigInt,
  era: string;
  hash: string;
  slot: PositiveBigInt,
  slotInEpoch: PositiveBigInt,
  slotsToEpochEnd: PositiveBigInt,
  syncProgress: number;
};

export namespace Tip {
  export const jsonDeserialiser: JsonDeserialiser<Tip> = jsonCodecs.objectOfDeserialiser({
    block: PositiveBigInt.jsonCodec.deserialise,
    epoch: PositiveBigInt.jsonCodec.deserialise,
    era: jsonCodecs.json2StringCodec.deserialise,
    hash: jsonCodecs.json2StringCodec.deserialise,
    slot: PositiveBigInt.jsonCodec.deserialise,
    slotInEpoch: PositiveBigInt.jsonCodec.deserialise,
    slotsToEpochEnd: PositiveBigInt.jsonCodec.deserialise,
    syncProgress: jsonCodecs.json2NumberCodec.deserialise,
  });
}

export const getTip = (debug: boolean = false): Tip => {
  const tip: Tip = unwrapOrPanicWith(
    execCardanoCliJsonTyped(
      [
        "query", "tip",
        ["--testnet-magic", 1097911063],
        "--output-json",
      ],
      Tip.jsonDeserialiser,
      debug,
    ),
    (e) => `Failed to query tip: ${JSON.stringify(e)}`,
  );
  const syncProgress = typeof tip.syncProgress === 'string' ? parseFloat(tip.syncProgress) : tip.syncProgress;
  if (syncProgress < 1) {
    throw new Error(`Node is not fully synchronized (${syncProgress}%)`);
  }
  return tip;
};

const setNetworkArg = (args: CliArgs, networkMagicNumber?: NetworkMagicNumber): void => {
  if (networkMagicNumber) {
    if(networkMagicNumber !== NetworkMagicNumber.MAINNET) {
      args.push(["--testnet-magic", networkMagicNumber]);
    } else {
      args.push("--mainnet");
    }
  }
}

export const queryUtxosByAddress = (address: AddressBech32, networkMagicNumber?: NetworkMagicNumber, debug: boolean = false): Result<UtxoMap, CommandError | JsonError> => {
  const args: CliArgs = [
    "query", "utxo",
    ["--address", address],
    "--output-json"
  ];
  setNetworkArg(args, networkMagicNumber);
  return execCardanoCliJsonTyped(
    args,
    UtxoMap.jsonDeserialiser,
    debug,
  );
};

export const getTxIdFromTxFile = (
  txFilePath: Path,
  debug = false
): Result<TxIdHex, CommandError | JsonError> => {
  return execCardanoCliJsonTyped(
    [
      "conway", "transaction", "txid",
      ["--tx-file", txFilePath],
    ],
    jsonCodecs.objectOf({ txhash: TxIdHex.jsonCodec, }).deserialise,
    debug
  ).map(obj => obj.txhash);
};

export const getTxIdFromTxBodyFile = (
  txBodyFilePath: Path,
  debug = false
): Result<TxIdHex, CommandError | JsonError> => {
  return execCardanoCliJsonTyped(
    [
      "conway", "transaction", "txid",
      ["--tx-body-file", txBodyFilePath],
    ],
    TxIdHex.jsonCodec.deserialise,
    debug
  );
};

export const getTxId = (txHex: TxHex, tmpDir: Path | null = null): Result<TxIdHex, CommandError | JsonError> => {
  const rootTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
  const txTmpDir = `${rootTmpDir}/txid-${Date.now()}-${Math.floor(Math.random() * 1000000)}`;
  nodeFs.mkdirSync(txTmpDir, { recursive: true });
  const txFilePath = `${txTmpDir}/tx.json`;
  const txJson = {
    type: "Tx ConwayEra",
    description: "Ledger Cddl Format",
    cborHex: txHex,
  };
  nodeFs.writeFileSync(txFilePath, JSON.stringify(txJson));
  return getTxIdFromTxFile(txFilePath);
};

export type TimeoutError = {
  type: "TimeoutError";
  message: string;
  timeout: Milliseconds;
};

export type WaitForUtxoError = CommandError | string | TimeoutError | JsonError;

export const waitForUtxo: (
  txOutRef: TxOutRefHex,
  debug?: boolean,
  delayMs?: number,
  timeoutMs?: number,
) => ResultAsync<TxOut, WaitForUtxoError> = fromSafePromiseResultFn(async (txOutRef, debug = false, delayMs = 1000, timeoutMs = 200_000,) => {
  const startTime = Date.now();
  const loop = async (): Promise<Result<TxOut, WaitForUtxoError>> => {
    if(Date.now() - startTime < timeoutMs) {
      return execCardanoCliJsonTyped(
        [
          "conway", "query", "utxo",
          ["--tx-in", txOutRef],
          "--output-json",
        ],
        UtxoMap.jsonDeserialiser,
        debug,
      ).match(
        async (utxos) => {
          const entries = UtxoMap.toUtxos(utxos).filter(utxo => utxo.txOutRef === txOutRef);
          if (entries.length === 1) {
            return ok(entries[0]!.txOut);
          }
          await new Promise(resolve => setTimeout(resolve, delayMs));
          return loop();
        },
        async e => err(e)
      );
    }
    return err({
      type: "TimeoutError",
      message: `Utxo ${txOutRef} did not appear within ${timeoutMs}ms`,
      timeout: timeoutMs,
    } as TimeoutError);
  }
  return loop();
});

const flattenTxId = (txId: TxId | TxIdHex): TxId => {
  if (typeof txId === 'string') {
    return TxIdHex.toTxId(txId);
  } else {
    return txId;
  }
}

// This function assumes that there is no other consumer of a given
// transaction output #0 as it awaits for that particular UTxO.
export const waitForTx = (
  // Either uint8array or hex string
  txIdOrTxIdRef: TxId | TxIdHex,
  debug = false,
  delayMs = 1000,
  timeoutMs = 200_000,
): ResultAsync<null, WaitForUtxoError> => {
  const txId = flattenTxId(txIdOrTxIdRef);
  console.log(`[cardanoCli] wait-for-tx: txid=${txId} timeoutMs=${timeoutMs}`);
  const txOutRef = TxOutRefHex.fromTxOutRef({ txId, txIx: TxIx.fromDigits(0) });
  const start = Date.now();
  return waitForUtxo(txOutRef, debug, delayMs, timeoutMs)
    .map((): null => {
      console.log(`[cardanoCli] wait-for-tx-confirmed: txid=${txId} elapsedMs=${Date.now() - start}`);
      return null;
    })
    .mapErr((err) => {
      console.error(`[cardanoCli] wait-for-tx-timeout: txid=${txId} elapsedMs=${Date.now() - start} err=${String(err)}`);
      return err;
    });
};

export const signTxEnvelope = function (
  skeyFile: string,
  txEnvelope: TxEnvelope,
  debug = false,
  tmpDir: Path | null = null, // When null we will use system temp dir
): Result<TxEnvelope, jsonCodecs.JsonError | CommandError | string> {
  const txTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
  const txEnvelopeJson = jsonCodecs.mkStringifier(TxEnvelope.jsonCodec)(txEnvelope);
  const unsignedTxFile = `${txTmpDir}/unsigned.tx.json`;
  const signedTxFile = `${txTmpDir}/sined.tx.json`;
  nodeFs.writeFileSync(unsignedTxFile, txEnvelopeJson);
  return execCardanoCli(
    [
      "conway", "transaction", "sign",
      ["--tx-file", unsignedTxFile],
      ["--signing-key-file", skeyFile],
      ["--out-file", signedTxFile],
    ],
    debug,
  )
    .andThen(() => {
      const signedTxJsonStr = nodeFs.readFileSync(signedTxFile, "utf8");
      return jsonCodecs.mkParser(TxEnvelope.jsonCodec)(signedTxJsonStr);
    });
};

export const signTx = function (
  skeyFile: string,
  txHex: TxHex,
  tmpDir: Path | null = null, // When null we will use system temp dir
  debug = false,
): Result<TxHex, CommandError | JsonError> {
  return getTxId(txHex).andThen(txIdHex => {
    // Create temp directory for transaction signing
    const rootTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
    const txTmpDir = `${rootTmpDir}/tx-${txIdHex}`;
    nodeFs.mkdirSync(txTmpDir, { recursive: true });

    // Prepare transaction body for signing
    const txBodyJson = {
      type: "Unwitnessed Tx ConwayEra",
      description: "Ledger Cddl Format",
      cborHex: txHex,
    };
    const txBodyFile = `${txTmpDir}/tx.body.json`;
    const txSignedFile = `${txTmpDir}/tx.signed.json`;
    nodeFs.writeFileSync(txBodyFile, JSON.stringify(txBodyJson));

    // Sign the transaction
    return execCardanoCli(
      [
        "conway", "transaction", "sign",
        ["--tx-body-file", txBodyFile],
        ["--signing-key-file", skeyFile],
        ["--out-file", txSignedFile],
      ],
      debug,
    ).map(() => {
      const signedTxJsonStr = nodeFs.readFileSync(txSignedFile, "utf8");
      const signedTxJson = JSON.parse(signedTxJsonStr);
      const signedTxHex: TxHex = signedTxJson.cborHex;
      return signedTxHex;
    });
  });
};

export type TxEnvelopeFile = Tagged<Path, 'TxEnvelopeFile'>;

const TX_DIAG_DIR = `${process.env.ROOT_DIR || process.cwd()}/marlowe-integration/test-temp/failed-txs`;

const persistFailedTx = (txFile: Path, label: string): void => {
  try {
    nodeFs.mkdirSync(TX_DIAG_DIR, { recursive: true });
    const ts = Date.now();
    const dest = `${TX_DIAG_DIR}/${label}-${ts}-${path.basename(txFile)}`;
    nodeFs.copyFileSync(txFile, dest);
    console.error(`[cardanoCli] persisted failed tx-body to ${dest}`);
  } catch (e) {
    console.error(`[cardanoCli] could not persist failed tx: ${String(e)}`);
  }
};

export const submitTxFromEnvelopeFile = (
  txFile: Path,
  debug = false,
  intervalMs = 1_000,
  timeoutMs = 180_000,
): ResultAsync<TxIdHex, WaitForUtxoError> => {
  console.log(`[cardanoCli] submitTxFromEnvelopeFile: txFile=${txFile}`);
  return toAsync(execCardanoCli(
    [
      "conway", "transaction", "submit",
      ["--tx-file", txFile],
    ],
    debug,
  ))
    .andThen(() => toAsync(getTxIdFromTxFile(txFile, debug)))
    .andThen((txId) => {
      console.log(`[cardanoCli] submit-ok: txid=${txId} file=${txFile}`);
      return waitForTx(txId, debug, intervalMs, timeoutMs).map(() => txId);
    })
    .mapErr((err) => {
      console.error(`[cardanoCli] submit-fail: file=${txFile} err=${String(err)}`);
      persistFailedTx(txFile, "submit-fail");
      return err;
    });
};

export const submitTxEnvelope = (
  txEnvelope: TxEnvelope,
  debug = false,
  tmpDir: Path | null = null,
  intervalMs = 1_000,
  timeoutMs = 180_000,
): ResultAsync<TxIdHex, WaitForUtxoError> => {
  const txEnvelopeJson = jsonCodecs.mkStringifier(TxEnvelope.jsonCodec)(txEnvelope);
  const rootTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
  const txTmpDir = `${rootTmpDir}/tx-submit-${Date.now()}-${Math.floor(Math.random() * 1000000)}`;
  nodeFs.mkdirSync(txTmpDir, { recursive: true });
  const txEnvelopeFile = `${txTmpDir}/tx.json`;
  nodeFs.writeFileSync(txEnvelopeFile, txEnvelopeJson);
  return submitTxFromEnvelopeFile(txEnvelopeFile, debug, intervalMs, timeoutMs);
}

export type Era = "conway" | "babbage" | "dijkstra";

export const submitTx = (
  txHex: TxHex,
  era: Era = "conway",
  tmpDir: Path | null = null,
  intervalMs = 1_000,
  timeoutMs = 180_000,
  debug = true,
): ResultAsync<TxIdHex, WaitForUtxoError> => {
  return toAsync(getTxId(txHex))
    .andThen((txIdHex) => {
      // Create temp directory for transaction submission
      const rootTmpDir = tmpDir || `${nodeOs.tmpdir()}`;
      const txTmpDir = `${rootTmpDir}/tx-submit-${txIdHex}`;
      nodeFs.mkdirSync(txTmpDir, { recursive: true });
      const txTag = (() => {
        switch (era) {
          case "conway":
            return "Tx ConwayEra";
          case "babbage":
            return "Tx BabbageEra";
          case "dijkstra":
            return "Tx DijkstraEra";
        }
      })();
      // Prepare transaction body for submission
      const txEnvelope = {
        type: txTag,
        description: "Ledger Cddl Format",
        cborHex: txHex,
      };
      const txEnvelopeFile = `${txTmpDir}/tx.json`;
      nodeFs.writeFileSync(txEnvelopeFile, JSON.stringify(txEnvelope));
      return submitTxFromEnvelopeFile(txEnvelopeFile, debug, intervalMs, timeoutMs);
  });
}

export const selectUtxos = (
  amountLovelace: number,
  utxoMap: UtxoMap,
): {
  selectedUtxos: UtxoMap;
  totalLovelace: number;
} => {
  const amountLovelaceBig = BigInt(amountLovelace);
  const sortedUtxos = [...utxoMap.entries()].sort(
    ([_aKey, a], [_bKey, b]) => {
      const aL = a.value.lovelace as unknown as bigint;
      const bL = b.value.lovelace as unknown as bigint;
      if (bL > aL) return 1;
      if (bL < aL) return -1;
      return 0;
    },
  );
  const selectedUtxos: UtxoMap = new Map<TxOutRefHex, TxOut>() as UtxoMap;
  let totalLovelaceBig = 0n;
  for (const [txOutRef, utxo] of sortedUtxos) {
    selectedUtxos.set(txOutRef, utxo);
    totalLovelaceBig += utxo.value.lovelace as unknown as bigint;
    if (totalLovelaceBig >= amountLovelaceBig) {
      break;
    }
  }
  if (totalLovelaceBig < amountLovelaceBig) {
    throw new Error(
      `Insufficient funds: required ${amountLovelace}, available ${totalLovelaceBig.toString()}`,
    );
  }
  return { selectedUtxos, totalLovelace: Number(totalLovelaceBig) };
};

const MAX_TRANSFER_RECIPIENTS = 60;
export type Recipient = {
  amount: number;
  address: AddressBech32
};

export async function transferFunds(
  walletAddress: AddressBech32,
  walletSkeyFile: Path,
  recipients: Recipient[],
  tmpDir: Path | null = null,
  networkMagicNumber?: NetworkMagicNumber,
  debug = false,
): Promise<Result<TxIdHex, CommandError | JsonError>> {
  if (recipients.length > MAX_TRANSFER_RECIPIENTS)
    throw new Error(
      `transferFunds supports up to ${MAX_TRANSFER_RECIPIENTS} UTxOs. Requested: ${recipients.length}`,
    );

  let walletTmpDir: Path;
  if (tmpDir) {
    walletTmpDir = tmpDir;
    nodeFs.mkdirSync(walletTmpDir, { recursive: true });
  } else {
    walletTmpDir = `${nodeOs.tmpdir()}/split-utxos-${Date.now()}-${Math.floor(
      Math.random() * 1000000,
    )}`;
    nodeFs.mkdirSync(walletTmpDir, { recursive: true });
  }

  const socketPath = process.env.CARDANO_NODE_SOCKET_PATH;
  const appendSocketPathArg = (args: CliArgs): void => {
    if (socketPath) {
      args.push(["--socket-path", socketPath]);
    }
  };

  const queryArgs: CliArgs = [
    "conway", "query", "utxo",
    ["--address", walletAddress],
    "--output-json",
  ];
  setNetworkArg(queryArgs, networkMagicNumber);
  appendSocketPathArg(queryArgs);

  const initialUtxoMap: UtxoMap = unwrapOrPanicWith(
    execCardanoCliJsonTyped(
      queryArgs,
      UtxoMap.jsonDeserialiser,
      debug,
    ),
    (e) => `Failed to query utxo by address ${walletAddress}: ${JSON.stringify(e)}`,
  );

  const totalSplit = recipients.reduce((sum, r) => sum + r.amount, 0);
  const selectedUtxos = selectUtxos(totalSplit, initialUtxoMap);

  const unsigedTxFile = `${walletTmpDir}/split.body.json`;
  const buildArgs: CliArgs = [
    "conway", "transaction", "build",
    ...Array.from(selectedUtxos.selectedUtxos.keys()).map((txOutRef) => ["--tx-in", txOutRef] as CliArg),
    ...recipients.map((recipient) => ["--tx-out", `${recipient.address}+${recipient.amount}`] as CliArg),
    ["--change-address", walletAddress],
    ["--out-file", unsigedTxFile],
  ];
  setNetworkArg(buildArgs, networkMagicNumber);
  appendSocketPathArg(buildArgs);
  execCardanoCli(buildArgs, debug);

  const signedTxFile = `${walletTmpDir}/split.tx.json`;

  execCardanoCli(
    [
      "conway", "transaction", "sign",
      ["--signing-key-file", walletSkeyFile],
      ["--tx-body-file", unsigedTxFile],
      ["--out-file", signedTxFile],
    ],
    debug,
  );

  console.log(`[cardanoCli] transferFunds: built unsigned tx, recipients=${recipients.length} totalLovelace=${totalSplit}`);
  await submitTxFromEnvelopeFile(signedTxFile, debug);
  const txId = getTxIdFromTxFile(signedTxFile, debug);
  console.log(`[cardanoCli] transferFunds: split txId=${txId} file=${signedTxFile}`);
  return txId;
}

// | Generate a fresh payment-only wallet on the given network and return its
// address plus the path to its signing key. The wallet files are written under
// `tmpDir` (or a fresh system temp dir if not provided) — callers that need
// cleanup are responsible for removing it.
export const createWallet = (
  networkMagicNumber: NetworkMagicNumber,
  tmpDir: Path | null = null,
  debug = false,
): Result<Wallet, CommandError | JsonError> => {
  const walletTmpDir =
    tmpDir ?? `${nodeOs.tmpdir()}/wallet-${Date.now()}-${Math.floor(Math.random() * 1000000)}`;
  nodeFs.mkdirSync(walletTmpDir, { recursive: true });

  const vkeyFile = `${walletTmpDir}/wallet.vkey`;
  const skeyFile = `${walletTmpDir}/wallet.skey`;
  const addrFile = `${walletTmpDir}/wallet.addr`;

  return execCardanoCli(
    [
      "conway", "address", "key-gen",
      ["--verification-key-file", vkeyFile],
      ["--signing-key-file", skeyFile],
    ],
    debug,
  ).andThen(() => {
    const buildArgs: CliArgs = [
      "conway", "address", "build",
      ["--payment-verification-key-file", vkeyFile],
      ["--out-file", addrFile],
    ];
    setNetworkArg(buildArgs, networkMagicNumber);

    return execCardanoCli(buildArgs, debug);
  }).map(() => {
    const addrStr = nodeFs.readFileSync(addrFile, "utf8").trim();
    const addr = unwrapOrPanicWith(
      AddressBech32.fromString(addrStr),
      (e) => `Failed to parse wallet address ${addrStr}: ${e}`,
    );

    return { addr, skeyFile: skeyFile as TaggedPath };
  });
};

