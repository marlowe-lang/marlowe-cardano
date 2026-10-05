import * as marloweRuntimeCli from '../../marloweRuntimeCli.js';
import type { Result } from 'neverthrow';
import { unwrapOrPanicWith } from '@konduit/konduit-consumer/neverthrow';
import type { Json } from '@konduit/codec/json';
import { stringify as jsonStringify } from '@konduit/codec/json';
import type { PostContractSourceResponse } from '@marlowe-lang/runtime/client';
import type { MarloweRuntimeConfig } from '../../marloweRuntimeCli.js';

type RunOpts = {
  runtime: MarloweRuntimeConfig;
};

// Exercises the simplest possible contract source: a `Close` wrapped in a
// `main` labelled object bundle. The runtime should:
//   1. accept the upload and return a `contractSourceId`,
//   2. return the same contract back from `GET /contracts/sources/{id}`,
//   3. report an empty adjacency set,
//   4. report a closure containing exactly the source itself.
export const run = async (opts: RunOpts): Promise<void> => {
  const { runtime } = opts;
  const bundle = [
    { label: 'main', type: 'contract', value: 'close' },
  ];

  const uploadResult: Result<PostContractSourceResponse, unknown> = await marloweRuntimeCli.runUploadContractSource(
    bundle,
    'main',
    runtime,
    {},
    null,
    true,
  );

  const uploaded = unwrapOrPanicWith(
    uploadResult,
    (err): string => `Failed to upload Close bundle: ${jsonStringify(err as Json)}`,
  );

  if (!uploaded.contractSourceId || uploaded.contractSourceId.length !== 64) {
    throw new Error(`Expected a 64-hex-char contractSourceId, got: ${uploaded.contractSourceId}`);
  }
  if (!uploaded.intermediateIds.main) {
    throw new Error(`Expected the uploaded bundle to advertise a 'main' intermediate id`);
  }
  if (uploaded.intermediateIds.main !== uploaded.contractSourceId) {
    throw new Error(
      `Expected contractSourceId (${uploaded.contractSourceId}) === intermediateIds.main (${uploaded.intermediateIds.main})`,
    );
  }

  const sourceId = uploaded.contractSourceId;

  // GET raw (no expand)
  const rawResult = await marloweRuntimeCli.runGetContractSource(
    sourceId,
    runtime,
    { expand: false },
    null,
    true,
  );
  const raw = unwrapOrPanicWith(
    rawResult,
    err => `Failed to GET raw contract source: ${jsonStringify(err)}`,
  );
  if (raw !== 'close') {
    throw new Error(`Expected raw source to equal "close", got: ${JSON.stringify(raw)}`);
  }

  // GET expanded (the demerkleized form should also be "close")
  const expandedResult = await marloweRuntimeCli.runGetContractSource(
    sourceId,
    runtime,
    { expand: true },
    null,
    true,
  );
  const expanded = unwrapOrPanicWith(
    expandedResult,
    err => `Failed to GET expanded contract source: ${jsonStringify(err)}`,
  );
  if (expanded !== 'close') {
    throw new Error(`Expected expanded source to equal "close", got: ${JSON.stringify(expanded)}`);
  }

  // Adjacency: a self-contained `Close` has no neighbours.
  const adjacencyResult = await marloweRuntimeCli.runGetContractSourceAdjacency(
    sourceId,
    runtime,
    null,
    true,
  );
  const adjacency = unwrapOrPanicWith(
    adjacencyResult,
    err => `Failed to GET adjacency: ${jsonStringify(err)}`,
  );
  if (adjacency.length !== 0) {
    throw new Error(`Expected no adjacent sources for a Close, got: ${JSON.stringify(adjacency)}`);
  }

  // Closure: should contain only the source itself.
  const closureResult = await marloweRuntimeCli.runGetContractSourceClosure(
    sourceId,
    runtime,
    null,
    true,
  );
  const closure = unwrapOrPanicWith(
    closureResult,
    err => `Failed to GET closure: ${jsonStringify(err)}`,
  );
  if (closure.length !== 1 || closure[0] !== sourceId) {
    throw new Error(
      `Expected closure to contain only [${sourceId}], got: ${JSON.stringify(closure)}`,
    );
  }
};