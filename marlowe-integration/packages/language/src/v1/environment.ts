import {
  json2BigIntCodec,
  objectOf,
  type JsonCodec,
} from "@konduit/codec/json/codecs";

export type TimeInterval = {
  from: bigint;
  to: bigint;
};

export function TimeInterval(from: bigint, to: bigint): TimeInterval {
  return { from, to };
}
export namespace TimeInterval {
  export const jsonCodec: JsonCodec<TimeInterval> = objectOf({
    from: json2BigIntCodec,
    to: json2BigIntCodec,
  });
}

export type Environment = {
  timeInterval: TimeInterval;
};

export function Environment(timeInterval: TimeInterval): Environment {
  return { timeInterval };
}
export namespace Environment {
  export const jsonCodec: JsonCodec<Environment> = objectOf({
    timeInterval: TimeInterval.jsonCodec,
  });
}

// `mkEnvironment` curried helper preserved for backward compatibility.
export const mkEnvironment =
  (start: Date) =>
  (end: Date): Environment =>
    Environment({ from: BigInt(start.getTime()), to: BigInt(end.getTime()) });