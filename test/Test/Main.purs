module Test.Main where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, message)
import Node.SQLite (DB, RunResult, Statement, all, close, defaultOptions, exec, get, openInMemory, prepare, prepare_, run)
import Node.SQLite as SQLite
import Node.SQLite.Encoding (class Encode)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Type.Proxy (Proxy(..))

-- | The error type flowing through the query helpers below.
type Query a = ExceptT SQLite.SQLiteError Aff a

-- | Opens a fresh, empty in-memory database, failing the test if it can't open.
withFreshDB :: (DB -> Aff Unit) -> Aff Unit
withFreshDB action = do
  dbResult <- liftEffect $ openInMemory defaultOptions
  case dbResult of
    Left err -> fail $ "DB open failed: " <> message err
    Right db -> action db

-- | Opens an in-memory database pre-populated with the `fruits` fixture.
withDB :: (DB -> Aff Unit) -> Aff Unit
withDB action = do
  dbResult <- liftEffect setupDB
  case dbResult of
    Left err -> fail $ "DB setup failed: " <> message err
    Right db -> action db

-- | Runs a query, failing the test on error, otherwise handing the result to
-- | the given assertion.
shouldSucceed :: forall a. Query a -> (a -> Aff Unit) -> Aff Unit
shouldSucceed action assert =
  runExceptT action >>= case _ of
    Left err -> fail $ "Unexpected query error: " <> show err
    Right a -> assert a

-- | `prepare` lifted into the `Query` monad.
prepareQ
  :: forall iR oR
   . DB
  -> Proxy iR
  -> String
  -> Proxy oR
  -> Query (Statement iR oR)
prepareQ db pin sql pout =
  withExceptT SQLite.SQLite'Exception $ ExceptT $ liftEffect $ prepare db pin sql pout

-- | `run` lifted into the `Query` monad.
runQ :: forall iR. Encode { | iR } => Statement iR () -> { | iR } -> Query RunResult
runQ stmt params =
  withExceptT SQLite.SQLite'Exception $ ExceptT $ liftEffect $ run stmt params

setupDB :: Effect (Either Error DB)
setupDB = runExceptT do
  db <- ExceptT $ openInMemory defaultOptions
  ExceptT $ exec db schema
  pure db
  where
  schema =
    """
    CREATE TABLE fruits (name TEXT, quantity INT) STRICT;
    INSERT INTO fruits VALUES ('Apple', 158);
    INSERT INTO fruits VALUES ('Banana', 158);
    INSERT INTO fruits VALUES ('Cherry', 162);
    INSERT INTO fruits VALUES ('Date', 165);
    """

spec :: Spec Unit
spec =
  describe "Node.SQLite" do

    describe "prepare" do
      it "fails on invalid SQL" $ withFreshDB \db -> do
        result <- liftEffect $ prepare_ db "THIS IS NOT SQL;"
        case result of
          Left _ -> pure unit
          Right _ -> fail "expected prepare to reject invalid SQL"

    describe "exec" do
      it "runs a multi-statement script" $ withFreshDB \db -> do
        execResult <- liftEffect $ exec db
          "CREATE TABLE t (x INT) STRICT; INSERT INTO t VALUES (10); INSERT INTO t VALUES (20);"
        case execResult of
          Left err -> fail $ "exec failed: " <> message err
          Right _ ->
            shouldSucceed
              ( do
                  stmt <- prepareQ db (Proxy @()) "SELECT x FROM t ORDER BY x;" (Proxy @(x :: Number))
                  all stmt {}
              )
              \rows -> rows `shouldEqual` [ { x: 10.0 }, { x: 20.0 } ]

    describe "run" do
      it "reports the number of changed rows" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE t (x INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @()) "INSERT INTO t VALUES (1), (2), (3);" (Proxy @())
              runQ insert {}
          )
          \result -> result.changes `shouldEqual` 3

      it "reports a positive lastInsertRowid" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE t (x INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @()) "INSERT INTO t VALUES (1);" (Proxy @())
              runQ insert {}
          )
          \result -> (result.lastInsertRowid > 0.0) `shouldEqual` true

      it "binds String and Int parameters" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE people (name TEXT, age INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(name :: String, age :: Int)) "INSERT INTO people VALUES (:name, :age);" (Proxy @())
              _ <- runQ insert { name: "Alice", age: 30 }
              select <- prepareQ db (Proxy @()) "SELECT name, age FROM people;" (Proxy @(name :: String, age :: Number))
              all select {}
          )
          \rows -> rows `shouldEqual` [ { name: "Alice", age: 30.0 } ]

      it "binds Boolean parameters as 0 and 1" $ withFreshDB \db ->
        -- We store a `Boolean` param into an INT column and read it back as a
        -- `Number`. A correctly encoded `false`/`true` lands as SQLite integers
        -- 0/1, so `ORDER BY flag` puts "off" first and the decoded numbers are
        -- 0.0/1.0. (An un-encoded Boolean would not become an integer.)
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE flags (label TEXT, flag INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, flag :: Boolean)) "INSERT INTO flags VALUES (:label, :flag);" (Proxy @())
              _ <- runQ insert { label: "on", flag: true }
              _ <- runQ insert { label: "off", flag: false }
              select <- prepareQ db (Proxy @()) "SELECT label, flag FROM flags ORDER BY flag;" (Proxy @(label :: String, flag :: Number))
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "off", flag: 0.0 }
            , { label: "on", flag: 1.0 }
            ]

      it "binds Just parameters as the inner value and Nothing as NULL" $ withFreshDB \db ->
        -- `DecodeSQLField (Maybe v)` is not implemented, so we cannot read a NULL
        -- column back into PureScript. Instead we let SQLite verify what was
        -- stored: `WHERE val = 42` matches only if `Just 42` encoded to the
        -- integer 42, and `WHERE val IS NULL` matches only if `Nothing` encoded
        -- to real SQL NULL. Only the plain `label` string is ever decoded.
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE opt (label TEXT, val INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, val :: Maybe Int)) "INSERT INTO opt VALUES (:label, :val);" (Proxy @())
              _ <- runQ insert { label: "present", val: Just 42 }
              _ <- runQ insert { label: "absent", val: Nothing }
              present <- prepareQ db (Proxy @()) "SELECT label FROM opt WHERE val = 42;" (Proxy @(label :: String))
              justRow <- all present {}
              missing <- prepareQ db (Proxy @()) "SELECT label FROM opt WHERE val IS NULL;" (Proxy @(label :: String))
              nullRow <- all missing {}
              pure { justRow, nullRow }
          )
          \{ justRow, nullRow } -> do
            justRow `shouldEqual` [ { label: "present" } ]
            nullRow `shouldEqual` [ { label: "absent" } ]

    describe "all" do
      it "returns all rows" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepareQ db (Proxy @()) "SELECT name, quantity FROM fruits ORDER BY name;" (Proxy @(name :: String, quantity :: Number))
              all stmt {}
          )
          \rows -> rows `shouldEqual`
            [ { name: "Apple", quantity: 158.0 }
            , { name: "Banana", quantity: 158.0 }
            , { name: "Cherry", quantity: 162.0 }
            , { name: "Date", quantity: 165.0 }
            ]

      it "returns an empty array when no rows match" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepareQ db (Proxy @()) "SELECT name, quantity FROM fruits WHERE quantity = 999;" (Proxy @(name :: String, quantity :: Number))
              all stmt {}
          )
          \rows -> rows `shouldEqual` ([] :: Array { name :: String, quantity :: Number })

      it "filters rows with an Int parameter" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepareQ db (Proxy @(quantity :: Int)) "SELECT name, quantity FROM fruits WHERE quantity = :quantity ORDER BY name;" (Proxy @(name :: String, quantity :: Number))
              all stmt { quantity: 158 }
          )
          \rows -> rows `shouldEqual`
            [ { name: "Apple", quantity: 158.0 }
            , { name: "Banana", quantity: 158.0 }
            ]

    describe "get" do
      it "returns the first matching row" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepareQ db (Proxy @()) "SELECT name, quantity FROM fruits WHERE quantity = 165;" (Proxy @(name :: String, quantity :: Number))
              get stmt {}
          )
          \row -> row `shouldEqual` Just { name: "Date", quantity: 165.0 }

      it "filters with an Int parameter" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepareQ db (Proxy @(quantity :: Int)) "SELECT name, quantity FROM fruits WHERE quantity = :quantity;" (Proxy @(name :: String, quantity :: Number))
              get stmt { quantity: 165 }
          )
          \row -> row `shouldEqual` Just { name: "Date", quantity: 165.0 }

      it "returns Nothing when no row matches" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepareQ db (Proxy @()) "SELECT name, quantity FROM fruits WHERE quantity = 999;" (Proxy @(name :: String, quantity :: Number))
              get stmt {}
          )
          \row -> row `shouldEqual` (Nothing :: Maybe { name :: String, quantity :: Number })

    describe "parameters requiring encoding" do
      -- `all` and `get` mirror `run`: they encode params before binding, so
      -- params whose runtime representation is not a native SQLite bind type
      -- (e.g. Boolean, Maybe) are converted correctly.
      it "binds a Boolean parameter through all" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE flags (label TEXT, flag INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, flag :: Boolean)) "INSERT INTO flags VALUES (:label, :flag);" (Proxy @())
              _ <- runQ insert { label: "on", flag: true }
              _ <- runQ insert { label: "off", flag: false }
              select <- prepareQ db (Proxy @(flag :: Boolean)) "SELECT label FROM flags WHERE flag = :flag;" (Proxy @(label :: String))
              all select { flag: true }
          )
          \rows -> rows `shouldEqual` [ { label: "on" } ]

      it "binds a Maybe parameter through all" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE opt (label TEXT, val INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, val :: Maybe Int)) "INSERT INTO opt VALUES (:label, :val);" (Proxy @())
              _ <- runQ insert { label: "present", val: Just 42 }
              _ <- runQ insert { label: "absent", val: Nothing }
              select <- prepareQ db (Proxy @(val :: Maybe Int)) "SELECT label FROM opt WHERE val = :val;" (Proxy @(label :: String))
              all select { val: Just 42 }
          )
          \rows -> rows `shouldEqual` [ { label: "present" } ]

      it "binds a Boolean parameter through get" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE flags (label TEXT, flag INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, flag :: Boolean)) "INSERT INTO flags VALUES (:label, :flag);" (Proxy @())
              _ <- runQ insert { label: "on", flag: true }
              _ <- runQ insert { label: "off", flag: false }
              select <- prepareQ db (Proxy @(flag :: Boolean)) "SELECT label FROM flags WHERE flag = :flag;" (Proxy @(label :: String))
              get select { flag: true }
          )
          \row -> row `shouldEqual` Just { label: "on" }

      it "binds a Maybe parameter through get" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE opt (label TEXT, val INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, val :: Maybe Int)) "INSERT INTO opt VALUES (:label, :val);" (Proxy @())
              _ <- runQ insert { label: "present", val: Just 42 }
              _ <- runQ insert { label: "absent", val: Nothing }
              select <- prepareQ db (Proxy @(val :: Maybe Int)) "SELECT label FROM opt WHERE val = :val;" (Proxy @(label :: String))
              get select { val: Just 42 }
          )
          \row -> row `shouldEqual` Just { label: "present" }

    describe "decoding Int" do
      it "decodes an INT column directly as Int" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepareQ db (Proxy @()) "SELECT name, quantity FROM fruits ORDER BY name;" (Proxy @(name :: String, quantity :: Int))
              all stmt {}
          )
          \rows -> rows `shouldEqual`
            [ { name: "Apple", quantity: 158 }
            , { name: "Banana", quantity: 158 }
            , { name: "Cherry", quantity: 162 }
            , { name: "Date", quantity: 165 }
            ]

      it "rounds a non-integer REAL column to the nearest Int" $ withFreshDB \db ->
        -- The `DecodeNonNull Int` instance reads the column as a `Number` and
        -- applies `round`, so fractional SQLite REAL values must come back as
        -- the nearest integer (ties rounding towards positive infinity).
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE measures (label TEXT, value REAL) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, value :: Number)) "INSERT INTO measures VALUES (:label, :value);" (Proxy @())
              _ <- runQ insert { label: "down", value: 1.4 }
              _ <- runQ insert { label: "up", value: 1.6 }
              _ <- runQ insert { label: "half", value: 2.5 }
              select <- prepareQ db (Proxy @()) "SELECT label, value FROM measures ORDER BY label;" (Proxy @(label :: String, value :: Int))
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "down", value: 1 }
            , { label: "half", value: 3 }
            , { label: "up", value: 2 }
            ]

    describe "decoding Boolean" do
      it "round-trips a Boolean through an INT column" $ withFreshDB \db ->
        -- `EncodeNonNull Boolean` stores false/true as 0/1 and
        -- `DecodeNonNull Boolean` reads the column as a `Number`, treating 0 as
        -- false and any other value as true.
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE flags (label TEXT, flag INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, flag :: Boolean)) "INSERT INTO flags VALUES (:label, :flag);" (Proxy @())
              _ <- runQ insert { label: "on", flag: true }
              _ <- runQ insert { label: "off", flag: false }
              select <- prepareQ db (Proxy @()) "SELECT label, flag FROM flags ORDER BY label;" (Proxy @(label :: String, flag :: Boolean))
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "off", flag: false }
            , { label: "on", flag: true }
            ]

      it "decodes a non-zero INT column as true" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE flags (label TEXT, flag INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, flag :: Int)) "INSERT INTO flags VALUES (:label, :flag);" (Proxy @())
              _ <- runQ insert { label: "two", flag: 2 }
              _ <- runQ insert { label: "zero", flag: 0 }
              select <- prepareQ db (Proxy @()) "SELECT label, flag FROM flags ORDER BY label;" (Proxy @(label :: String, flag :: Boolean))
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "two", flag: true }
            , { label: "zero", flag: false }
            ]

    describe "Char fields" do
      it "round-trips a Char through a TEXT column" $ withFreshDB \db ->
        -- `EncodeNonNull Char` stores the character as a single-character TEXT
        -- and `DecodeNonNull Char` reads it back via the first code unit, so a
        -- bound `Char` param must decode to the same `Char`.
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE grades (label TEXT, grade TEXT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, grade :: Char)) "INSERT INTO grades VALUES (:label, :grade);" (Proxy @())
              _ <- runQ insert { label: "alice", grade: 'A' }
              _ <- runQ insert { label: "bob", grade: 'B' }
              select <- prepareQ db (Proxy @()) "SELECT label, grade FROM grades ORDER BY label;" (Proxy @(label :: String, grade :: Char))
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "alice", grade: 'A' }
            , { label: "bob", grade: 'B' }
            ]

      it "decodes only the first character of a multi-character TEXT column" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE grades (label TEXT, grade TEXT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, grade :: String)) "INSERT INTO grades VALUES (:label, :grade);" (Proxy @())
              _ <- runQ insert { label: "alice", grade: "ABC" }
              select <- prepareQ db (Proxy @()) "SELECT label, grade FROM grades;" (Proxy @(label :: String, grade :: Char))
              get select {}
          )
          \row -> row `shouldEqual` Just { label: "alice", grade: 'A' }

      it "fails to decode an empty TEXT column as Char" $ withFreshDB \db -> do
        result <- runExceptT do
          create <- prepareQ db (Proxy @()) "CREATE TABLE grades (label TEXT, grade TEXT) STRICT;" (Proxy @())
          _ <- runQ create {}
          insert <- prepareQ db (Proxy @(label :: String, grade :: String)) "INSERT INTO grades VALUES (:label, :grade);" (Proxy @())
          _ <- runQ insert { label: "alice", grade: "" }
          select <- prepareQ db (Proxy @()) "SELECT label, grade FROM grades;" (Proxy @(label :: String, grade :: Char))
          get select {}
        case result of
          Left _ -> pure unit
          Right _ -> fail "expected decoding an empty string as Char to fail"

    describe "decoding Maybe" do
      it "decodes a non-null INT column as Just and a NULL one as Nothing" $ withFreshDB \db ->
        -- Round-trips both cases through storage: `Just 42` is stored as the
        -- integer 42 and must decode back to `Just 42.0`, while `Nothing` is
        -- stored as SQL NULL and must decode back to `Nothing`.
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE opt (label TEXT, val INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, val :: Maybe Int)) "INSERT INTO opt VALUES (:label, :val);" (Proxy @())
              _ <- runQ insert { label: "present", val: Just 42 }
              _ <- runQ insert { label: "absent", val: Nothing }
              select <- prepareQ db (Proxy @()) "SELECT label, val FROM opt ORDER BY label;" (Proxy @(label :: String, val :: Maybe Number))
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "absent", val: Nothing }
            , { label: "present", val: Just 42.0 }
            ]

      it "decodes a nullable TEXT column as Maybe String" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE notes (id INT, note TEXT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(id :: Int, note :: Maybe String)) "INSERT INTO notes VALUES (:id, :note);" (Proxy @())
              _ <- runQ insert { id: 1, note: Just "hello" }
              _ <- runQ insert { id: 2, note: Nothing }
              select <- prepareQ db (Proxy @()) "SELECT id, note FROM notes ORDER BY id;" (Proxy @(id :: Number, note :: Maybe String))
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { id: 1.0, note: Just "hello" }
            , { id: 2.0, note: Nothing }
            ]

      it "decodes a Maybe column through get" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepareQ db (Proxy @()) "CREATE TABLE opt (label TEXT, val INT) STRICT;" (Proxy @())
              _ <- runQ create {}
              insert <- prepareQ db (Proxy @(label :: String, val :: Maybe Int)) "INSERT INTO opt VALUES (:label, :val);" (Proxy @())
              _ <- runQ insert { label: "absent", val: Nothing }
              _ <- runQ insert { label: "present", val: Just 7 }
              select <- prepareQ db (Proxy @()) "SELECT label, val FROM opt WHERE label = 'absent';" (Proxy @(label :: String, val :: Maybe Number))
              get select {}
          )
          \row -> row `shouldEqual` Just { label: "absent", val: (Nothing :: Maybe Number) }

    describe "close" do
      it "closes the database without error" $ withFreshDB \db -> do
        closeResult <- liftEffect $ close db
        case closeResult of
          Left err -> fail $ "close failed: " <> message err
          Right _ -> pure unit

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

