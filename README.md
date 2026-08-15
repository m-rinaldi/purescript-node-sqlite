# purescript-node-sqlite

A typed PureScript API for Node's built-in [`node:sqlite`](https://nodejs.org/api/sqlite.html) module: prepared statements are indexed by row types, so parameter binding and row decoding go through the compiler instead of raw `Foreign` values, and a mismatch between your declared types and what SQLite returns surfaces as a `SQLite'DecodeError` instead of corrupted data.

Since **v22.5.0**, Node.js embeds SQLite natively and exposes it through the `node:sqlite` module, so there is no external database engine to install or link against.

No native dependencies and nothing to compile: SQLite ships with Node itself. Prepared statements carry the shapes of their input parameters and output rows in their type, so parameter binding and row decoding are checked by the compiler.

## Requirements

- Node.js **>= 22.5.0** (the version that introduced `node:sqlite`)
- PureScript **>= 0.15.16**

Because `node:sqlite` is currently experimental, Node prints an experimental-feature warning at runtime. You can silence it with `NODE_NO_WARNINGS=1` or `node --no-warnings`.

## Installation

```sh
spago install node-sqlite
```

## Quick start

```purescript
module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Node.SQLite as SQLite
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = do
  result <- SQLite.openInMemory SQLite.defaultOptions
  case result of
    Left err -> logShow err
    Right db -> do
      -- Create a table.
      _ <- SQLite.exec db "CREATE TABLE fruits (name TEXT, quantity INT) STRICT;"

      -- Insert rows with a prepared statement.
      insert <- SQLite.prepare db
        (Proxy @(name :: String, quantity :: Int))
        "INSERT INTO fruits VALUES (:name, :quantity);"
        (Proxy @())
      case insert of
        Left err -> logShow err
        Right stmt -> do
          _ <- SQLite.run stmt { name: "apple", quantity: 3 }
          _ <- SQLite.run stmt { name: "pear", quantity: 5 }
          pure unit

      -- Query rows back.
      select <- SQLite.prepare db
        (Proxy @(quantity :: Int))
        "SELECT name, quantity FROM fruits WHERE quantity >= :quantity;"
        (Proxy @(name :: String, quantity :: Int))
      case select of
        Left err -> logShow err
        Right stmt -> do
          rows <- runExceptT (SQLite.all stmt { quantity: 4 })
          logShow rows -- Right [{ name: "pear", quantity: 5 }]

      _ <- SQLite.close db
      pure unit
```

## API overview

### Opening and closing

| Function | Description |
| --- | --- |
| `open :: String -> Options -> Effect (Either Error DB)` | Open a database file at the given path. |
| `openInMemory :: Options -> Effect (Either Error DB)` | Open a temporary in-memory database. |
| `close :: DB -> Effect (Either Error Unit)` | Close the connection. |
| `defaultOptions :: Options` | Sensible defaults (foreign keys on, defensive flag on). |

`Options` mirrors the fields accepted by `node:sqlite`'s `DatabaseSync` constructor: `readOnly`, `enableForeignKeyConstraints`, `enableDoubleQuotedStringLiterals`, `allowExtension`, `timeout`, `allowUnknownNamedParameters`, and `defensive`.

### Statements

Prepared statements are typed `Statement inputRow outputRow`. The two `Proxy` arguments to `prepare` fix the input-parameter row and the output-column row:

| Function | Description |
| --- | --- |
| `prepare :: DB -> Proxy inputR -> String -> Proxy outputR -> Effect (Either Error (Statement inputR outputR))` | Compile SQL with explicit input/output row types. |
| `prepare' :: DB -> String -> Proxy outputR -> Effect (Either Error (Statement () outputR))` | Compile SQL that takes no parameters. |
| `prepare_ :: DB -> String -> Effect (Either Error (Statement () ()))` | Compile SQL with neither parameters nor result columns. |

### Executing

| Function | Description |
| --- | --- |
| `exec :: DB -> String -> Effect (Either Error Unit)` | Run a raw (possibly multi-statement) SQL script with no parameters. Good for migrations. |
| `run :: Statement iR () -> { \| iR } -> Effect (Either Error RunResult)` | Run an `INSERT`/`UPDATE`/`DELETE`. Returns `{ changes, lastInsertRowid }`. |
| `all :: Statement iR r -> { \| iR } -> ExceptT SQLiteError m (Array { \| r })` | Run a row-returning statement and collect every row. |
| `get :: Statement iR r -> { \| iR } -> ExceptT SQLiteError m (Maybe { \| r })` | Run a row-returning statement and take the first row, if any. |

`all` and `get` run in `ExceptT SQLiteError m` so a decoding failure and a SQLite exception share one error channel:

```purescript
data SQLiteError
  = SQLite'Exception Error        -- thrown by the SQLite engine
  | SQLite'DecodeError Decoding.Error  -- rows didn't match the expected type
```

## Supported column types

Parameters and result columns are mapped between PureScript and SQLite as follows:

| PureScript | SQLite |
| --- | --- |
| `Int` | INTEGER |
| `Number` | REAL (an INTEGER column is rounded to `Int` on decode) |
| `String` | TEXT |
| `Char` | TEXT (single character) |
| `Boolean` | INTEGER (`false` ↔ 0, `true` ↔ non-zero) |
| `Maybe a` | the inner value, or NULL for `Nothing` |

## Development

```sh
spago build      # compile the library
spago test       # run the spec suite (requires Node >= 22.5.0)
```

## License

[MIT](./LICENSE) © Jorge Rinaldi
