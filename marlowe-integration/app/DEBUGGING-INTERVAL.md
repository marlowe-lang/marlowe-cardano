# Debugging Interval problem

I execute two commands so we have two Json files:

```shell
cabal run marlowe-runtime:cli -- contract get --contract-id aa67bb60665e3d7fc7114aee6138f08cbda884f9fc538f5641e1821486953868#1  --message-format json > get-response.json

cabal run marlowe-runtime:cli -- contract next --contract-id aa67bb60665e3d7fc7114aee6138f08cbda884f9fc538f5641e1821486953868#1 --validity-start 2026-08-31T11:26:40.257Z --validity-end 2026-08-31T11:36:40.257Z --message-format json 2> next-response.json
```

When I inspect the error in the next-response.json I can see:

```
"invalidInterval": {
    "from": 1788175600257,
    "to": 1788176200257
}
```

which seems to be really invalid given that in the contract state JSON file we can find timeout:

```json
    "currentContract": {
        "timeout": 1788176109796,
        "timeout_continuation": "close",
        ...
    }
```

and (timeout = 1788176109796) < (validity-end = 1788176200257) (we are not proving by this interval that the timeout is in the future.

Could you please check what those POSIX timestamps mean - are they correct given what we have in the CLI? 
