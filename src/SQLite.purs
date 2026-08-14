module Node.SQLite
  ( DB
  , Error(..)
  , Options
  , RunResult
  , Statement
  , all
  , close
  , defaultOptions
  , exec
  , get
  , open
  , openInMemory
  , prepare
  , prepare'
  , prepare_
  , run
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), mapExceptT, withExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (try)
import Effect.Exception as Exception
import Foreign (Foreign)
import Prim.RowList (class RowToList)
import SQLite.Decoding (class DecodeSQL, decode)
import SQLite.Decoding as Decoding
import SQLite.Encoding (class Encode, SQLEncodedRecord, encode)
import Type.Proxy (Proxy(..))

-- |
-- | Options for `open`.
-- |
-- | * `readOnly`: If true, the database is opened in read-only mode. If the database does not exist, opening it will fail.
-- | * `enableForeignKeyConstraints`: If true, foreign key constraints are enabled. This is recommended but can be disabled for compatibility with legacy database schemas. The enforcement of foreign key constraints can be enabled and disabled after opening the database using PRAGMA foreign_keys.
-- | * `enableDoubleQuotedStringLiterals`: If true, SQLite will accept double-quoted string literals. This is not recommended but can be enabled for compatibility with legacy database schemas.
-- | * `allowExtension`: If true, the `loadExtension` SQL function and the `loadExtension()` method are enabled.
-- | * `timeout` The busy timeout in milliseconds. This is the maximum amount of time that SQLite will wait for a database lock to be released before returning an error.
-- | * `allowUnknownNamedParameters`: If true, unknown named parameters are ignored when binding. If false, an exception is thrown for unknown named parameters.
-- | * `defensive`: If true, enables the defensive flag. When the defensive flag is enabled, language features that allow ordinary SQL to deliberately corrupt the database file are disabled.
type Options =
  { readOnly :: Boolean
  , enableForeignKeyConstraints :: Boolean
  , enableDoubleQuotedStringLiterals :: Boolean
  , allowExtension :: Boolean
  , timeout :: Int
  , allowUnknownNamedParameters :: Boolean
  , defensive :: Boolean
  }

-- | Default options for `open`. Foreign keys are enabled and the defensive flag is on;
-- | everything else is disabled and the busy timeout is 0.
defaultOptions :: Options
defaultOptions =
  { readOnly: false
  , enableForeignKeyConstraints: true
  , enableDoubleQuotedStringLiterals: false
  , allowExtension: false
  , timeout: 0
  , allowUnknownNamedParameters: false
  , defensive: true
  }

-- | Opens a database file at the given path with the provided options.
open :: String -> Options -> Effect (Either Exception.Error DB)
open opts = openImpl opts >>> try

-- | Opens a temporary in-memory database. The database is gone when closed.
openInMemory :: Options -> Effect (Either Exception.Error DB)
openInMemory = open ":memory:"

-- | An opaque handle to an open SQLite database.
foreign import data DB :: Type

foreign import openImpl :: String -> Options -> Effect DB

-- | Closes the database connection.
close :: DB -> Effect (Either Exception.Error Unit)
close = closeImpl >>> try

foreign import closeImpl :: DB -> Effect Unit

-- | An opaque handle to a compiled prepared statement.
foreign import data Statement :: Row Type -> Row Type -> Type

-- | Compiles a SQL string into a prepared statement.
-- |
-- | Statements are always tied to a specific DB connection.
prepare
  :: ∀ (inputR :: Row Type) (outputR :: Row Type)
   . DB
  -> Proxy inputR
  -> String
  -> Proxy outputR
  -> Effect (Either Exception.Error (Statement inputR outputR))
prepare db _ sql _ = try (prepareImpl db sql)

prepare' :: ∀ outputR. DB -> String -> Proxy outputR -> Effect (Either Exception.Error (Statement () outputR))
prepare' db sql = prepare db (Proxy @()) sql

prepare_ :: DB -> String -> Effect (Either Exception.Error (Statement () ()))
prepare_ db sql = prepare' db sql (Proxy @())

foreign import prepareImpl :: ∀ iR oR. DB -> String -> Effect (Statement iR oR)

-- | Result returned by `run`.
-- |
-- | * `changes`: number of rows affected by the statement.
-- | * `lastInsertRowid`: rowid of the last `INSERT` on this connection; unchanged (not reset) for non-`INSERT` statements.
type RunResult = { changes :: Int, lastInsertRowid :: Number }

-- | runs a prepared statement.
-- |
-- | It does not return rows, used for `INSERT`, `UPDATE` and `DELETE`.
run
  :: ∀ (iR :: Row Type)
   . Encode { | iR }
  => Statement iR ()
  -> { | iR }
  -> Effect (Either Exception.Error RunResult)
run stmt params = runImpl stmt encodedParams # try
  where
  encodedParams = encode params

foreign import runImpl :: ∀ (iR :: Row Type). Statement iR () -> SQLEncodedRecord -> Effect RunResult

-- | `exec` takes a raw SQL string and executes it directly (no preparation, no parameters), and returns nothing.
-- |
-- | Good for multi-statement scripts like schema migrations.
exec :: DB -> String -> Effect (Either Exception.Error Unit)
exec db sql = try (execImpl db sql)

foreign import execImpl :: DB -> String -> Effect Unit

-- | Errors that can occur when running a query.
-- |
-- | * `SQLiteException`: an error thrown by the underlying SQLite engine.
-- | * `DecodeError`: the rows returned by SQLite could not be decoded into the expected PureScript type.
data Error
  = SQLiteException Exception.Error
  | DecodeError Decoding.Error

instance Show Error where
  show (SQLiteException err) = "Exception from to Node's SQLite API: " <> show err
  show (DecodeError err) = "Decoding error: " <> show err

-- | `all` runs a `SELECT` (or any row-returning statement, e.g. a `RETURNING`
-- | clause) and returns every matching row as an array. Use it when you expect
-- | zero or more rows.
all
  :: ∀ @r rl m inputR
   . RowToList r rl
  => DecodeSQL (Array (Record r))
  => MonadEffect m
  => Statement inputR r
  -> Record inputR
  -> ExceptT Error m (Array (Record r))
all stmt params = do
  -- TODO params are passed unencoded to the FFI. Unlike `run`, there is no
  -- `Encode` constraint / `encode` call. This works only for types whose
  -- runtime representation already matches what SQLite expects (Int, Number,
  -- String), which is why the current Int-param tests pass. It breaks for types
  -- that need conversion, e.g. `Maybe v` (Nothing should become null) or
  -- Boolean (true should become 1).
  -- Fix (mirroring `run`): 1) add an `Encode (Record inputR)` constraint,
  -- 2) `let encodedParams = encode params` and pass that to `allImpl`,
  -- 3) change `allImpl` to accept `SQLEncodedRecord` instead of `Record iR`.
  let tryAll = try (allImpl stmt params)
  foreignResult <- tryAll <#> lmap SQLiteException # ExceptT # mapExceptT liftEffect
  withExceptT DecodeError (decode foreignResult)

foreign import allImpl :: ∀ iR oR. Statement iR oR -> Record iR -> Effect Foreign

-- | `get` runs a `SELECT` (or any row-returning statement) and returns the
-- | first matching row, or `Nothing` if no rows matched. Use it for lookups
-- | that yield at most one row (e.g. a primary-key or `LIMIT 1` query).
get
  :: ∀ @r rl m iR
   . RowToList r rl
  => DecodeSQL (Maybe (Record r))
  => MonadEffect m
  => Statement iR r
  -> Record iR
  -> ExceptT Error m (Maybe (Record r))
get stmt params = do
  -- TODO params are passed unencoded to the FFI. Unlike `run`, there is no
  -- `Encode` constraint / `encode` call. This works only for types whose
  -- runtime representation already matches what SQLite expects (Int, Number,
  -- String), which is why the current Int-param tests pass. It breaks for types
  -- that need conversion, e.g. `Maybe v` (Nothing should become null) or
  -- Boolean (true should become 1).
  -- Fix (mirroring `run`): 1) add an `Encode (Record iR)` constraint,
  -- 2) `let encodedParams = encode params` and pass that to `getImpl`,
  -- 3) change `getImpl` to accept `SQLEncodedRecord` instead of `Record iR`.
  let tryGet = try (getImpl stmt params)
  foreignResult <- tryGet <#> lmap SQLiteException # ExceptT # mapExceptT liftEffect
  withExceptT DecodeError (decode foreignResult)

foreign import getImpl :: ∀ iR oR. Statement iR oR -> Record iR -> Effect Foreign