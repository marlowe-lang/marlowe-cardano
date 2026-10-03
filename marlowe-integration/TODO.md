# Add narrowing `is*` helpers to all the unions

I started introducing `is*` helpers and `match` function to every type associated namespace like:

```ts
export namespace Contract {
  export const isAssert = (contract: Contract): contract is Assert => typeof contract === "object" && "assert" in contract;
  export const isClose = (contract: Contract): contract is Close => contract === "close";
  export const isIf = (contract: Contract): contract is If => typeof contract === "object" && "if" in contract;
  export const isLet = (contract: Contract): contract is Let => typeof contract === "object" && "let" in contract;
  export const isPay = (contract: Contract): contract is Pay => typeof contract === "object" && "pay" in contract;
  export const isWhen = (contract: Contract): contract is When => typeof contract === "object" && "when" in contract;

  export const match = <T>(contract: Contract, handlers: { assert: (assert: Assert) => T, close: (close: Close) => T, if: (if_: If) => T, let: (let_: Let) => T, pay: (pay: Pay) => T, when: (when: When) => T }): T =>
    isAssert(contract) ? handlers.assert(contract)
    : isClose(contract) ? handlers.close(contract)
    : isIf(contract) ? handlers.if(contract)
    : isLet(contract) ? handlers.let(contract)
    : isPay(contract) ? handlers.pay(contract)
    : handlers.when(contract)

  export const areEqual = areEqualThunk<Contract>(() => (a, b) =>
    isAssert(a) ? isAssert(b) && Assert.areEqual(a, b)
    : isClose(a) ? isClose(b) && Close.areEqual(a, b)
    : isIf(a) ? isIf(b) && If.areEqual(a, b)
    : isLet(a) ? isLet(b) && Let.areEqual(a, b)
    : isPay(a) ? isPay(b) && Pay.areEqual(a, b)
    : isWhen(b) && When.areEqual(a, b)
  );

  export const jsonCodec: JsonCodec<Contract> = fromCodecThunkFn(() =>
    altJsonCodecs<[JsonCodec<Close>, JsonCodec<Pay>, JsonCodec<If>, JsonCodec<When>, JsonCodec<Let>, JsonCodec<Assert>]>(
      [Close.jsonCodec, Pay.jsonCodec, If.jsonCodec, When.jsonCodec, Let.jsonCodec, Assert.jsonCodec],
      (serClose, serPay, serIf, serWhen, serLet, serAssert) => (contract: Contract) => match(contract, {
          close: serClose,
          pay: serPay,
          if: serIf,
          when: serWhen,
          let: serLet,
          assert: serAssert
      })
    )
  );
}
```

As you can see I also refactored and used those new functions in `areEqual` and `jsonCodec` functions. Could you please go over the language package and refactor all the unions to use this pattern? I believe that current ./my-custom-bin/tsc actually fails on related bug which you gonna fix during this refactoring.

Good luck
