import { execSync } from "child_process";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import * as yargs from "yargs";
import { bech32 } from "bech32";
import {
  Envelope,
  TestVector,
  ShelleyWallet,
  ShelleyAddress,
} from "cli-claiming/src/cardano.js";

export type AddressType = 0 | 1 | 2 | 4 | 6 | 14;

const bech32ToHex = (value: string): string => {
  const decoded = bech32.decode(value, 1000);
  const data = bech32.fromWords(decoded.words);
  return Buffer.from(data).toString("hex");
};

function runCommand(command: string): string {
  return execSync(command).toString().trim();
}

export const mkShelleyWallet = async function (
  mnemonic: string,
  addressType: number,
  networkTag: number,
): Promise<ShelleyWallet> {
  // All file operations now happen in the current working directory,
  // which will be set to a temporary directory by the calling function.
  fs.writeFileSync("temp_mnemonic", mnemonic);
  runCommand(
    "cat temp_mnemonic | cardano-address key from-recovery-phrase Shelley > root.prv",
  );
  runCommand(
    "cat root.prv | cardano-address key child 1852H/1815H/0H/0/0 > addr.prv",
  );
  runCommand(
    "cat root.prv | cardano-address key child 1852H/1815H/0H/2/0 > stake.prv",
  );

  const addr = (() => {
    // This is now not extended key whatever it means: https://github.com/IntersectMBO/cardano-addresses/pull/134/files#r643021612
    runCommand(
      "cat stake.prv | cardano-address key public --without-chain-code > stake.pub",
    );
    runCommand(
      "cat addr.prv | cardano-address key public --without-chain-code > addr.pub",
    );
    runCommand(
      'cardano-address script hash "all [$(cat addr.pub)]" > script.hash',
    );

    switch (addressType) {
      case 0:
        runCommand(
          `cat addr.pub | cardano-address address payment --network-tag ${networkTag} | cardano-address address delegation $(cat stake.pub) > addr.pay`,
        );
        break;
      case 1:
        runCommand(
          `cat script.hash | cardano-address address payment --network-tag ${networkTag} | cardano-address address delegation $(cat stake.pub) > addr.pay`,
        );
        break;
      case 2:
        runCommand(
          `cat addr.pub | cardano-address address payment --network-tag ${networkTag} | cardano-address address delegation $(cat script.hash) > addr.pay`,
        );
        break;
      case 4:
        runCommand(
          `cat addr.pub | cardano-address address payment --network-tag ${networkTag} | cardano-address address pointer 42 14 0 > addr.pay`,
        );
        break;
      case 6:
        runCommand(
          `cat addr.pub | cardano-address address payment --network-tag ${networkTag} > addr.pay`,
        );
        break;
      case 14:
        runCommand(
          `cat stake.pub | cardano-address address stake --network-tag ${networkTag} > addr.pay`,
        );
        break;
      default:
        break;
    }
    let b: string = fs.readFileSync("addr.pay", "utf-8").trim();
    return { bech32: b, hex: bech32ToHex(b) };
  })();

  const keys = await (async () => {
    // as mentioned above prv is an extended key
    runCommand(
      "cardano-cli key convert-cardano-address-key --shelley-payment-key --signing-key-file addr.prv --out-file addr.skey",
    );
    runCommand(
      "cardano-cli key verification-key --signing-key-file addr.skey --verification-key-file addr.vkey",
    );
    const skey: Envelope = JSON.parse(fs.readFileSync("addr.skey", "utf-8"));
    const vkey: Envelope = JSON.parse(fs.readFileSync("addr.vkey", "utf-8"));
    const address = fs.readFileSync("addr.pub", "utf-8").trim();
    const shelleyFormat = { skey, vkey };
    const hex = {
      skey: bech32ToHex(fs.readFileSync("addr.prv", "utf-8").trim()),
      vkey: bech32ToHex(address),
    };
    const stakeAddress = fs.readFileSync("stake.pub", "utf-8").trim();
    const hexStake = {
      skey: bech32ToHex(fs.readFileSync("stake.prv", "utf-8").trim()),
      vkey: bech32ToHex(stakeAddress),
    };
    return { hex, hexStake, shelleyFormat };
  })();

  return { addr, keys, mnemonic };
};

export const generateShelleyWallet = async function (
  mnemonicSize: number = 15,
  addressType: AddressType,
  networkTag: number = 0,
): Promise<ShelleyWallet> {
  const mnemonic = runCommand(
    `cardano-address recovery-phrase generate --size ${mnemonicSize}`,
  );
  // This function is now just a wrapper and doesn't need to know about temp directories.
  return await mkShelleyWallet(mnemonic, addressType, networkTag);
};

async function generateTestVector(
  mnemonicSize: number = 15,
  addressType: AddressType,
  networkTag: number,
): Promise<TestVector> {
  const { addr, mnemonic, keys } = await generateShelleyWallet(
    mnemonicSize,
    addressType,
    networkTag,
  );
  const shade = Math.floor(Math.random() * 100) + 1;
  return { addr, mnemonic, keys, shade };
}

// This function now creates and manages the temporary directory
async function generateTestVectors(
  count: number,
  addressType: AddressType,
  networkTag: number,
  mnemonicSize?: number,
): Promise<TestVector[]> {
  const tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "cardano-batch-"));
  const originalDir = process.cwd();
  console.log(`Generating wallets in temporary directory: ${tempDir}`);

  try {
    // Change into the temporary directory
    process.chdir(tempDir);
    console.log(tempDir);

    const promises = Array.from({ length: count }, () =>
      generateTestVector(mnemonicSize, addressType, networkTag),
    );
    return await Promise.all(promises);
  } finally {
    // Always change back to the original directory and clean up
    process.chdir(originalDir);
    console.log(`Cleaning up temporary directory: ${tempDir}`);
    fs.rmSync(tempDir, { recursive: true, force: true });
  }
}

interface GenerateArgs {
  addressType: AddressType;
  claimNumber: number;
  networkTag: number;
  jsonFile?: string;
  mnemonicSize?: number;
}

interface ProvTreeCsvArgs {
  jsonFile: string;
  csvFile: string;
}

async function generateCommand(args: GenerateArgs) {
  try {
    const testVectors = await generateTestVectors(
      args.claimNumber,
      args.addressType,
      args.networkTag,
      args.mnemonicSize,
    );

    const jsonOutput = JSON.stringify(testVectors, null, 2);
    if (args.jsonFile) {
      fs.writeFileSync(args.jsonFile, jsonOutput);
      console.log(
        `Generated ${args.claimNumber} test vectors and saved to ${args.jsonFile}`,
      );
    } else {
      console.log(jsonOutput);
    }
  } catch (error) {
    console.error("An error occurred:", error);
    process.exit(1);
  }
}

function provTreeCsvCommand(args: ProvTreeCsvArgs) {
  try {
    const testVectors: TestVector[] = JSON.parse(
      fs.readFileSync(args.jsonFile, "utf-8"),
    );
    const csvContent = testVectors
      .map((tv) => {
        const addr = tv.addr as ShelleyAddress;
        return `cardano,${addr.bech32},${tv.shade}`;
      })
      .join("\n");
    fs.writeFileSync(args.csvFile, csvContent);
    console.log(`Generated CSV and saved to ${args.csvFile}`);
  } catch (error) {
    console.error("An error occurred:", error);
    process.exit(1);
  }
}

export const cli = (yargs: yargs.Argv) => {
  return yargs
    .command(
      "generate",
      "Generate new test vectors",
      (yargs) => {
        return yargs
          .option("claim-number", {
            alias: "n",
            description: "Number of claims",
            type: "number",
            demandOption: true,
          })
          .option("json-file", {
            alias: "j",
            description: "JSON file name",
            type: "string",
          })
          .option("mnemonic-size", {
            alias: "m",
            description: "Number of words in the mnemonic (12, 15, or 24)",
            type: "number",
            choices: [12, 15, 24],
            default: 12,
          })
          .option("address-type", {
            alias: "a",
            description: addressTypeDescription,
            type: "number",
            choices: [0, 1, 2, 4, 6, 14],
            default: 0,
            coerce: (arg) => arg as AddressType,
          })
          .option("network-tag", {
            alias: "t",
            description: "Network-tag number, should be set to 1 if is mainnet",
            type: "number",
            choices: [0, 1],
            default: 0,
          });
      },
      async (argv) => await generateCommand(argv as GenerateArgs),
    )
    .command(
      "prov-tree-csv",
      "Generate CSV from existing JSON file",
      (yargs) => {
        return yargs
          .option("json-file", {
            alias: "j",
            description: "Input JSON file name",
            type: "string",
            demandOption: true,
          })
          .option("csv-file", {
            alias: "c",
            description: "Output CSV file name",
            type: "string",
            demandOption: true,
          });
      },
      (argv) => provTreeCsvCommand(argv as ProvTreeCsvArgs),
    )
    .demandCommand(1, "You need to specify a command before moving on")
    .help()
    .alias("help", "h");
};

export const addressTypeDescription = `Address type according to CIP-19 (0, 1, 2, 4, 6, 14)
Header part | Payment part   | Delegation part
(0) 0000    | PaymentKeyHash | StakeKeyHash
(1) 0001    | ScriptHash     | StakeKeyHash
(2) 0010    | PaymentKeyHash | ScriptHash
(3) 0011    | ScriptHash     | ScriptHash
(4) 0100    | PaymentKeyHash | Pointer
(5) 0101    | ScriptHash     | Pointer
(6) 0110    | PaymentKeyHash | ø
(7) 0111    | ScriptHash     | ø
Header type | Stake Reference
(14) 1110   | StakeKeyHash`;
