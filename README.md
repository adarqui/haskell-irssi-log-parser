# haskell-irssi-log-parser

Port of https://github.com/kfranqueiro/node-irssi-log-parser.

Eventually this library will provide the following features:
[-] parse irssi log files
[-] export parsed irssi log data
[-] import parsed irssi log data
[-] parse raw irc messages. then the ability to export/import as json/irssi logs.

Right now i'm using the regex from node-irssi-log-parser to parse the irssi log file lines. Eventually i'll probably also provide Parsec functions.

## Tests

```
cabal install --enable-tests --only-dependencies && cabal configure --enable-tests && cabal build && cabal test
```

pc
