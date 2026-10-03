/**
 * This module offers utility functions to work with {@link https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map | Map}'s defined as ordered associative lists.
 * It is implemented as defined in the Marlowe spec.
 * @see {@link https://github.com/input-output-hk/marlowe/blob/master/isabelle/Util/MList.thy }
 * @packageDocumentation
 */

export type AssocMap<K, V> = Array<[K, V]>;

export type Sort = "GreaterThan" | "LowerThan" | "EqualTo";

export type Linorder<K> = (a: K, b: K) => Sort;

export function insert<K, V>(cmp: Linorder<K>, key: K, value: V, list: AssocMap<K, V>): AssocMap<K, V> {
  if (list.length === 0) {
    return [[key, value]];
  }
  const [head, ...tail] = list;
  const [headKey, _] = head;
  switch (cmp(key, headKey)) {
    case "LowerThan":
      return [[key, value], head, ...tail];
    case "GreaterThan":
      return [head, ...insert(cmp, key, value, tail)];
    case "EqualTo":
      return [[key, value], ...tail];
  }
}

export function remove<K, V>(cmp: Linorder<K>, key: K, list: AssocMap<K, V>): AssocMap<K, V> {
  if (list.length === 0) {
    return [];
  }
  const [head, ...tail] = list;
  const [headKey, _] = head;
  switch (cmp(key, headKey)) {
    case "EqualTo":
      return tail;
    case "GreaterThan":
      return [head, ...remove(cmp, key, tail)];
    case "LowerThan":
      return [head, ...tail];
  }
}

export function lookup<K, V>(cmp: Linorder<K>, key: K, list: AssocMap<K, V>): V | undefined {
  if (list.length === 0) {
    return undefined;
  }
  const [head, ...tail] = list;
  const [headKey, headValue] = head;
  switch (cmp(key, headKey)) {
    case "EqualTo":
      return headValue;
    case "GreaterThan":
      return lookup(cmp, key, tail);
    case "LowerThan":
      return undefined;
  }
}

export function findWithDefault<K, V>(cmp: Linorder<K>, defaultValue: V, key: K, list: AssocMap<K, V>): V {
  const value = lookup(cmp, key, list);
  return value === undefined ? defaultValue : value;
}

export function strCmp(a: string, b: string): Sort {
  if (a < b) {
    return "LowerThan";
  }
  if (a > b) {
    return "GreaterThan";
  }
  return "EqualTo";
}

export function member<K, V>(cmp: Linorder<K>, key: K, list: AssocMap<K, V>): boolean {
  return lookup(cmp, key, list) !== undefined;
}

// Structural array equality. Two arrays are equal when they have the same
// length and every element at index `i` compares equal under `eq`. The
// element comparator is the only piece of `areEqual` knowledge that varies
// per type, so the rest of the `*.areEqual` helpers can stay focused on
// their own shapes.
export function arrayAreEqualWith<T>(
  a: readonly T[],
  b: readonly T[],
  eq: (x: T, y: T) => boolean,
): boolean {
  if (a === b) return true;
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) {
    if (!eq(a[i] as T, b[i] as T)) return false;
  }
  return true;
}

// Lazy `areEqual` thunk — mirrors the lazy `fromCodecThunkFn` pattern used
// by the JSON codecs. The body is only evaluated the first time the
// returned comparator is invoked, which lets recursive types (`Value`,
// `Observation`, `Contract`, ...) close over their own namespaces that are
// not yet defined at module-evaluation time.
export function areEqualThunk<T>(
  thunk: () => (a: T, b: T) => boolean,
): (a: T, b: T) => boolean {
  let cached: ((a: T, b: T) => boolean) | undefined;
  return (a, b) => {
    if (cached === undefined) cached = thunk();
    return cached(a, b);
  };
}