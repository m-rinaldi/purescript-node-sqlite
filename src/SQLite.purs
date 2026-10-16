module Node.SQLite
  ( DB
  , SQLiteError(..)
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

import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (try)
import Effect.Exception as Exception
import Foreign (Foreign)
import Node.SQLite.Decoding (class Decode, decode)
import Node.SQLite.Decoding as Decoding
import Node.SQLite.Encoding (class Encode, EncodedRecord, encode)
import Prim.RowList (class RowToList)
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
open :: ∀ m. MonadEffect m => String -> Options -> ExceptT SQLiteError m DB
open path opts = liftSQLite (openImpl path opts)

-- | Opens a temporary in-memory database. The database is gone when closed.
openInMemory :: ∀ m. MonadEffect m => Options -> ExceptT SQLiteError m DB
openInMemory = open ":memory:"

-- | An opaque handle to an open SQLite database.
foreign import data DB :: Type

foreign import openImpl :: String -> Options -> Effect DB

-- | Closes the database connection.
close :: ∀ m. MonadEffect m => DB -> ExceptT SQLiteError m Unit
close db = liftSQLite (closeImpl db)

foreign import closeImpl :: DB -> Effect Unit

-- | An opaque handle to a compiled prepared statement.
foreign import data Statement :: Row Type -> Row Type -> Type

-- | Compiles a SQL string into a prepared statement.
-- |
-- | Statements are always tied to a specific DB connection.
prepare
  :: ∀ (inputR :: Row Type) (outputR :: Row Type) m
   . MonadEffect m
  => DB
  -> Proxy inputR
  -> String
  -> Proxy outputR
  -> ExceptT SQLiteError m (Statement inputR outputR)
prepare db _ sql _ = liftSQLite (prepareImpl db sql)

prepare' :: ∀ outputR m. MonadEffect m => DB -> String -> Proxy outputR -> ExceptT SQLiteError m (Statement () outputR)
prepare' db sql = prepare db (Proxy @()) sql

prepare_ :: ∀ m. MonadEffect m => DB -> String -> ExceptT SQLiteError m (Statement () ())
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
  :: ∀ (iR :: Row Type) m
   . Encode { | iR }
  => MonadEffect m
  => Statement iR ()
  -> { | iR }
  -> ExceptT SQLiteError m RunResult
run stmt params = liftSQLite (runImpl stmt encodedParams)
  where
  encodedParams = encode params

foreign import runImpl :: ∀ (iR :: Row Type). Statement iR () -> EncodedRecord -> Effect RunResult

-- | `exec` takes a raw SQL string and executes it directly (no preparation, no parameters), and returns nothing.
-- |
-- | Good for multi-statement scripts like schema migrations.
exec :: ∀ m. MonadEffect m => DB -> String -> ExceptT SQLiteError m Unit
exec db sql = liftSQLite (execImpl db sql)

foreign import execImpl :: DB -> String -> Effect Unit

-- | Errors that can occur when running a query.
-- |
-- | * `SQLite'Exception`: an error thrown by the underlying SQLite engine.
-- | * `SQLite'DecodeError`: the rows returned by SQLite could not be decoded into the expected PureScript type.
data SQLiteError
  = SQLite'Exception Exception.Error
  | SQLite'DecodeError Decoding.Error

instance Show SQLiteError where
  show (SQLite'Exception err) = "Exception from Node's SQLite API: " <> show err
  show (SQLite'DecodeError err) = "Decoding error: " <> show err

-- | `all` runs a `SELECT` (or any row-returning statement, e.g. a `RETURNING`
-- | clause) and returns every matching row as an array. Use it when you expect
-- | zero or more rows.
all
  :: ∀ @r rl m inputR
   . RowToList r rl
  => Decode (Array (Record r))
  => Encode (Record inputR)
  => MonadEffect m
  => Statement inputR r
  -> Record inputR
  -> ExceptT SQLiteError m (Array (Record r))
all = fetch allImpl

foreign import allImpl :: ∀ iR oR. Statement iR oR -> EncodedRecord -> Effect Foreign

-- | `get` runs a `SELECT` (or any row-returning statement) and returns the
-- | first matching row, or `Nothing` if no rows matched. Use it for lookups
-- | that yield at most one row (e.g. a primary-key or `LIMIT 1` query).
get
  :: ∀ @r rl m iR
   . RowToList r rl
  => Decode (Maybe (Record r))
  => Encode (Record iR)
  => MonadEffect m
  => Statement iR r
  -> Record iR
  -> ExceptT SQLiteError m (Maybe (Record r))
get = fetch getImpl

foreign import getImpl :: ∀ iR oR. Statement iR oR -> EncodedRecord -> Effect Foreign

-- | Shared body of `all` and `get`: encode the parameters, run the given FFI
-- | call through `liftSQLite`, and decode the `Foreign` result into `result`.
fetch
  :: ∀ iR oR result m
   . Encode (Record iR)
  => Decode result
  => MonadEffect m
  => (Statement iR oR -> EncodedRecord -> Effect Foreign)
  -> Statement iR oR
  -> Record iR
  -> ExceptT SQLiteError m result
fetch impl stmt params = do
  foreignResult <- liftSQLite (impl stmt encodedParams)
  withExceptT SQLite'DecodeError (decode foreignResult)
  where
  encodedParams = encode params

liftSQLite :: ∀ a m. MonadEffect m => Effect a -> ExceptT SQLiteError m a
liftSQLite call = try call <#> lmap SQLite'Exception # liftEffect # ExceptT