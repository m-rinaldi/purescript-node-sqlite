# purescript-node-sqlite

[![CI](https://github.com/m-rinaldi/purescript-node-sqlite/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/m-rinaldi/purescript-node-sqlite/actions/workflows/ci.yml)

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
import Effect (Effect)
import Effect.Class.Console (logShow)
import Node.SQLite as SQLite

main :: Effect Unit
main = do
  result <- runExceptT do
    db <- SQLite.openInMemory SQLite.defaultOptions

    -- Create a table.
    SQLite.exec db "CREATE TABLE fruits (name TEXT, quantity INT) STRICT;"

    -- Insert rows with a prepared statement.
    insert <- SQLite.prepare @(name :: String, quantity :: Int) @() db
      "INSERT INTO fruits VALUES (:name, :quantity);"
    _ <- SQLite.run insert { name: "apple", quantity: 3 }
    _ <- SQLite.run insert { name: "pear", quantity: 5 }

    -- Query rows back.
    select <- SQLite.prepare @(quantity :: Int) @(name :: String, quantity :: Int) db
      "SELECT name, quantity FROM fruits WHERE quantity >= :quantity;"
    rows <- SQLite.all select { quantity: 4 }
    SQLite.close db
    pure rows

  case result of
    Left err -> logShow err
    Right rows -> logShow rows -- [{ name: "pear", quantity: 5 }]
```

## API overview

### Opening and closing

| Function | Description |
| --- | --- |
| `open :: String -> Options -> ExceptT SQLiteError m DB` | Open a database file at the given path. |
| `openInMemory :: Options -> ExceptT SQLiteError m DB` | Open a temporary in-memory database. |
| `close :: DB -> ExceptT SQLiteError m Unit` | Close the connection. |
| `defaultOptions :: Options` | Sensible defaults (foreign keys on, defensive flag on). |

`Options` mirrors the fields accepted by `node:sqlite`'s `DatabaseSync` constructor: `readOnly`, `enableForeignKeyConstraints`, `enableDoubleQuotedStringLiterals`, `allowExtension`, `timeout`, `allowUnknownNamedParameters`, and `defensive`.

### Statements

Prepared statements are typed `Statement inputRow outputRow`. The two visible type arguments to `prepare` fix the input-parameter row and the output-column row, in that order. Use `()` for no parameters or no columns, and `@_` to leave one to inference where the context fixes it:

| Function | Description |
| --- | --- |
| `prepare @inputR @outputR :: DB -> String -> ExceptT SQLiteError m (Statement inputR outputR)` | Compile SQL with explicit input/output row types. |

### Executing

| Function | Description |
| --- | --- |
| `exec :: DB -> String -> ExceptT SQLiteError m Unit` | Run a raw (possibly multi-statement) SQL script with no parameters. Good for migrations. |
| `run :: Statement iR () -> { \| iR } -> ExceptT SQLiteError m RunResult` | Run an `INSERT`/`UPDATE`/`DELETE`. Returns `{ changes, lastInsertRowid }`. |
| `all :: Statement iR r -> { \| iR } -> ExceptT SQLiteError m (Array { \| r })` | Run a row-returning statement and collect every row. |
| `get :: Statement iR r -> { \| iR } -> ExceptT SQLiteError m (Maybe { \| r })` | Run a row-returning statement and take the first row, if any. |

### One-shot queries

For a statement that runs once, `prepare` and `all`/`get`/`run` can be one call. The parameter row is read off the record you pass; the result row is a visible type argument:

| Function | Description |
| --- | --- |
| `query @r :: DB -> String -> { \| iR } -> ExceptT SQLiteError m (Array { \| r })` | Prepare and run once, collecting every row. |
| `queryFirst @r :: DB -> String -> { \| iR } -> ExceptT SQLiteError m (Maybe { \| r })` | Prepare and run once, taking the first row, if any. |
| `queryOne @r :: DB -> String -> { \| iR } -> ExceptT SQLiteError m { \| r }` | Like `queryFirst`, for statements that always yield a row (`SELECT count(*) ...`, `INSERT ... RETURNING ...`). No row is a `SQLite'DecodeError`. |
| `command :: DB -> String -> { \| iR } -> ExceptT SQLiteError m RunResult` | Prepare and run once a statement that returns no rows (`INSERT`/`UPDATE`/`DELETE`). Returns `{ changes, lastInsertRowid }`. |

```purescript
names <- SQLite.query @(name :: String) db "SELECT name FROM fruits WHERE quantity > :n;" { n: 160 }
total <- _.total <$> SQLite.queryOne @(total :: Int) db "SELECT count(*) AS total FROM fruits;" {}
{ changes } <- SQLite.command db "DELETE FROM fruits WHERE quantity < :n;" { n: 160 }
```

The statement is compiled on every call; a statement that runs many times is still better prepared once.

Every function runs in `ExceptT SQLiteError m` (for any `MonadEffect m`), so a SQLite exception and a decoding failure share one error channel and calls compose in a single `do` block:

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
npm ci                            # install the pinned purs and spago from package-lock.json
spago build --pedantic-packages   # compile; fail on unused/missing deps (CI runs this)
spago test                        # run the spec suite (requires Node >= 22.5.0)
```

CI runs the build and tests on Node 22, 24 and 26 for every push to `main` and every pull request.

### Releasing

Releases are published to the [PureScript Registry](https://registry.purescript.org) by the
`Release` GitHub Actions workflow when a `vX.Y.Z` tag is pushed. Do **not** run `spago publish`
locally; just push the tag.

1. Bump `package.publish.version` in `spago.yaml` to `X.Y.Z` and run `spago build` so `spago.lock` is current.
2. Commit on `main`: `git commit -am "Release vX.Y.Z"`, then push.
3. Tag and push the tag: `git tag vX.Y.Z && git push origin vX.Y.Z`.

The workflow refuses to publish if the tag does not match `spago.yaml`, the tag is not on
`main`, or the build/tests fail. It then runs `spago publish` and creates a GitHub Release.
To rehearse without publishing, open Actions → Release → "Run workflow", select the tag,
and leave "Skip `spago publish`" ticked.

## License

[MIT](./LICENSE) © Jorge Rinaldi
