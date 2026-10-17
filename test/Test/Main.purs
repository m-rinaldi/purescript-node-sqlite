module Test.Main where

import Prelude

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Node.SQLite (DB, all, close, command, defaultOptions, exec, get, openInMemory, prepare, query, queryFirst, queryOne, run)
import Node.SQLite as SQLite
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- | The error type flowing through the query helpers below.
type Query a = ExceptT SQLite.SQLiteError Aff a

-- | Opens a fresh, empty in-memory database, failing the test if it can't open.
withFreshDB :: (DB -> Aff Unit) -> Aff Unit
withFreshDB action = do
  dbResult <- runExceptT $ openInMemory defaultOptions
  case dbResult of
    Left err -> fail $ "DB open failed: " <> show err
    Right db -> action db

-- | Opens an in-memory database pre-populated with the `fruits` fixture.
withDB :: (DB -> Aff Unit) -> Aff Unit
withDB action = do
  dbResult <- runExceptT setupDB
  case dbResult of
    Left err -> fail $ "DB setup failed: " <> show err
    Right db -> action db

-- | Runs a query, failing the test on error, otherwise handing the result to
-- | the given assertion.
shouldSucceed :: forall a. Query a -> (a -> Aff Unit) -> Aff Unit
shouldSucceed action assert =
  runExceptT action >>= case _ of
    Left err -> fail $ "Unexpected query error: " <> show err
    Right a -> assert a

setupDB :: Query DB
setupDB = do
  db <- openInMemory defaultOptions
  exec db schema
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
        result <- runExceptT $ prepare @() @() db "THIS IS NOT SQL;"
        case result of
          Left _ -> pure unit
          Right _ -> fail "expected prepare to reject invalid SQL"

      it "infers the input row from a wildcard" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepare @_ @(name :: String) db "SELECT name FROM fruits WHERE quantity = :quantity;"
              get stmt { quantity: 165 }
          )
          \row -> row `shouldEqual` Just { name: "Date" }

      it "infers both rows when the context fixes them" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepare db "SELECT name FROM fruits WHERE quantity = :quantity;"
              get stmt { quantity: 165 }
          )
          \row -> row `shouldEqual` Just { name: "Date" }

    describe "exec" do
      it "runs a multi-statement script" $ withFreshDB \db -> do
        execResult <- runExceptT $ exec db
          "CREATE TABLE t (x INT) STRICT; INSERT INTO t VALUES (10); INSERT INTO t VALUES (20);"
        case execResult of
          Left err -> fail $ "exec failed: " <> show err
          Right _ ->
            shouldSucceed
              ( do
                  stmt <- prepare @() @(x :: Number) db "SELECT x FROM t ORDER BY x;"
                  all stmt {}
              )
              \rows -> rows `shouldEqual` [ { x: 10.0 }, { x: 20.0 } ]

    describe "run" do
      it "reports the number of changed rows" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE t (x INT) STRICT;"
              _ <- run create {}
              insert <- prepare @() @() db "INSERT INTO t VALUES (1), (2), (3);"
              run insert {}
          )
          \result -> result.changes `shouldEqual` 3

      it "reports a positive lastInsertRowid" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE t (x INT) STRICT;"
              _ <- run create {}
              insert <- prepare @() @() db "INSERT INTO t VALUES (1);"
              run insert {}
          )
          \result -> (result.lastInsertRowid > 0.0) `shouldEqual` true

      it "binds String and Int parameters" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE people (name TEXT, age INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(name :: String, age :: Int) @() db "INSERT INTO people VALUES (:name, :age);"
              _ <- run insert { name: "Alice", age: 30 }
              select <- prepare @() @(name :: String, age :: Number) db "SELECT name, age FROM people;"
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
              create <- prepare @() @() db "CREATE TABLE flags (label TEXT, flag INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, flag :: Boolean) @() db "INSERT INTO flags VALUES (:label, :flag);"
              _ <- run insert { label: "on", flag: true }
              _ <- run insert { label: "off", flag: false }
              select <- prepare @() @(label :: String, flag :: Number) db "SELECT label, flag FROM flags ORDER BY flag;"
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "off", flag: 0.0 }
            , { label: "on", flag: 1.0 }
            ]

      it "binds Just parameters as the inner value and Nothing as NULL" $ withFreshDB \db ->
        -- `DecodeValue (Maybe v)` is not implemented, so we cannot read a NULL
        -- column back into PureScript. Instead we let SQLite verify what was
        -- stored: `WHERE val = 42` matches only if `Just 42` encoded to the
        -- integer 42, and `WHERE val IS NULL` matches only if `Nothing` encoded
        -- to real SQL NULL. Only the plain `label` string is ever decoded.
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE opt (label TEXT, val INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, val :: Maybe Int) @() db "INSERT INTO opt VALUES (:label, :val);"
              _ <- run insert { label: "present", val: Just 42 }
              _ <- run insert { label: "absent", val: Nothing }
              present <- prepare @() @(label :: String) db "SELECT label FROM opt WHERE val = 42;"
              justRow <- all present {}
              missing <- prepare @() @(label :: String) db "SELECT label FROM opt WHERE val IS NULL;"
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
              stmt <- prepare @() @(name :: String, quantity :: Number) db "SELECT name, quantity FROM fruits ORDER BY name;"
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
              stmt <- prepare @() @(name :: String, quantity :: Number) db "SELECT name, quantity FROM fruits WHERE quantity = 999;"
              all stmt {}
          )
          \rows -> rows `shouldEqual` ([] :: Array { name :: String, quantity :: Number })

      it "filters rows with an Int parameter" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepare @(quantity :: Int) @(name :: String, quantity :: Number) db "SELECT name, quantity FROM fruits WHERE quantity = :quantity ORDER BY name;"
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
              stmt <- prepare @() @(name :: String, quantity :: Number) db "SELECT name, quantity FROM fruits WHERE quantity = 165;"
              get stmt {}
          )
          \row -> row `shouldEqual` Just { name: "Date", quantity: 165.0 }

      it "filters with an Int parameter" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepare @(quantity :: Int) @(name :: String, quantity :: Number) db "SELECT name, quantity FROM fruits WHERE quantity = :quantity;"
              get stmt { quantity: 165 }
          )
          \row -> row `shouldEqual` Just { name: "Date", quantity: 165.0 }

      it "returns Nothing when no row matches" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepare @() @(name :: String, quantity :: Number) db "SELECT name, quantity FROM fruits WHERE quantity = 999;"
              get stmt {}
          )
          \row -> row `shouldEqual` (Nothing :: Maybe { name :: String, quantity :: Number })

    describe "query" do
      it "returns all rows" $ withDB \db ->
        shouldSucceed
          (query @(name :: String, quantity :: Number) db "SELECT name, quantity FROM fruits ORDER BY name;" {})
          \rows -> rows `shouldEqual`
            [ { name: "Apple", quantity: 158.0 }
            , { name: "Banana", quantity: 158.0 }
            , { name: "Cherry", quantity: 162.0 }
            , { name: "Date", quantity: 165.0 }
            ]

      it "returns an empty array when no rows match" $ withDB \db ->
        shouldSucceed
          (query @(name :: String) db "SELECT name FROM fruits WHERE quantity = 999;" {})
          \rows -> rows `shouldEqual` ([] :: Array { name :: String })

      it "takes its parameters from the record" $ withDB \db ->
        shouldSucceed
          (query @(name :: String) db "SELECT name FROM fruits WHERE quantity = :quantity ORDER BY name;" { quantity: 158 })
          \rows -> rows `shouldEqual` [ { name: "Apple" }, { name: "Banana" } ]

      it "fails on invalid SQL" $ withDB \db -> do
        result <- runExceptT $ query @(name :: String) db "THIS IS NOT SQL;" {}
        case result of
          Left (SQLite.SQLite'Exception _) -> pure unit
          Left err -> fail $ "expected SQLite'Exception, got: " <> show err
          Right _ -> fail "expected query to reject invalid SQL"

    describe "queryFirst" do
      it "returns the first matching row" $ withDB \db ->
        shouldSucceed
          (queryFirst @(name :: String, quantity :: Number) db "SELECT name, quantity FROM fruits WHERE quantity = :quantity;" { quantity: 165 })
          \row -> row `shouldEqual` Just { name: "Date", quantity: 165.0 }

      it "returns Nothing when no row matches" $ withDB \db ->
        shouldSucceed
          (queryFirst @(name :: String) db "SELECT name FROM fruits WHERE quantity = 999;" {})
          \row -> row `shouldEqual` (Nothing :: Maybe { name :: String })

    describe "queryOne" do
      it "returns the one row of an aggregate" $ withDB \db ->
        shouldSucceed
          (queryOne @(total :: Int) db "SELECT count(*) AS total FROM fruits;" {})
          \row -> row `shouldEqual` { total: 4 }

      it "returns the row of INSERT ... RETURNING" $ withDB \db ->
        shouldSucceed
          (queryOne @(name :: String, quantity :: Int) db "INSERT INTO fruits VALUES (:name, :quantity) RETURNING name, quantity;" { name: "Elderberry", quantity: 7 })
          \row -> row `shouldEqual` { name: "Elderberry", quantity: 7 }

      it "fails with a decode error when there is no row" $ withDB \db -> do
        result <- runExceptT $ queryOne @(name :: String) db "SELECT name FROM fruits WHERE quantity = 999;" {}
        case result of
          Left (SQLite.SQLite'DecodeError _) -> pure unit
          Left err -> fail $ "expected SQLite'DecodeError, got: " <> show err
          Right _ -> fail "expected queryOne to fail on no row"

    describe "command" do
      it "reports the number of changed rows" $ withDB \db ->
        shouldSucceed
          (command db "UPDATE fruits SET quantity = 0 WHERE quantity = :quantity;" { quantity: 158 })
          \{ changes } -> changes `shouldEqual` 2

      it "reports lastInsertRowid of an INSERT" $ withDB \db ->
        shouldSucceed
          ( do
              { lastInsertRowid } <- command db "INSERT INTO fruits VALUES (:name, :quantity);" { name: "Elderberry", quantity: 7 }
              row <- queryOne @(name :: String) db "SELECT name FROM fruits WHERE rowid = :rowid;" { rowid: lastInsertRowid }
              pure row.name
          )
          \name -> name `shouldEqual` "Elderberry"

      it "fails on invalid SQL" $ withDB \db -> do
        result <- runExceptT $ command db "THIS IS NOT SQL;" {}
        case result of
          Left (SQLite.SQLite'Exception _) -> pure unit
          Left err -> fail $ "expected SQLite'Exception, got: " <> show err
          Right _ -> fail "expected command to reject invalid SQL"

    describe "parameters requiring encoding" do
      -- `all` and `get` mirror `run`: they encode params before binding, so
      -- params whose runtime representation is not a native SQLite bind type
      -- (e.g. Boolean, Maybe) are converted correctly.
      it "binds a Boolean parameter through all" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE flags (label TEXT, flag INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, flag :: Boolean) @() db "INSERT INTO flags VALUES (:label, :flag);"
              _ <- run insert { label: "on", flag: true }
              _ <- run insert { label: "off", flag: false }
              select <- prepare @(flag :: Boolean) @(label :: String) db "SELECT label FROM flags WHERE flag = :flag;"
              all select { flag: true }
          )
          \rows -> rows `shouldEqual` [ { label: "on" } ]

      it "binds a Maybe parameter through all" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE opt (label TEXT, val INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, val :: Maybe Int) @() db "INSERT INTO opt VALUES (:label, :val);"
              _ <- run insert { label: "present", val: Just 42 }
              _ <- run insert { label: "absent", val: Nothing }
              select <- prepare @(val :: Maybe Int) @(label :: String) db "SELECT label FROM opt WHERE val = :val;"
              all select { val: Just 42 }
          )
          \rows -> rows `shouldEqual` [ { label: "present" } ]

      it "binds a Boolean parameter through get" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE flags (label TEXT, flag INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, flag :: Boolean) @() db "INSERT INTO flags VALUES (:label, :flag);"
              _ <- run insert { label: "on", flag: true }
              _ <- run insert { label: "off", flag: false }
              select <- prepare @(flag :: Boolean) @(label :: String) db "SELECT label FROM flags WHERE flag = :flag;"
              get select { flag: true }
          )
          \row -> row `shouldEqual` Just { label: "on" }

      it "binds a Maybe parameter through get" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE opt (label TEXT, val INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, val :: Maybe Int) @() db "INSERT INTO opt VALUES (:label, :val);"
              _ <- run insert { label: "present", val: Just 42 }
              _ <- run insert { label: "absent", val: Nothing }
              select <- prepare @(val :: Maybe Int) @(label :: String) db "SELECT label FROM opt WHERE val = :val;"
              get select { val: Just 42 }
          )
          \row -> row `shouldEqual` Just { label: "present" }

    describe "decoding Int" do
      it "decodes an INT column directly as Int" $ withDB \db ->
        shouldSucceed
          ( do
              stmt <- prepare @() @(name :: String, quantity :: Int) db "SELECT name, quantity FROM fruits ORDER BY name;"
              all stmt {}
          )
          \rows -> rows `shouldEqual`
            [ { name: "Apple", quantity: 158 }
            , { name: "Banana", quantity: 158 }
            , { name: "Cherry", quantity: 162 }
            , { name: "Date", quantity: 165 }
            ]

      it "rounds a non-integer REAL column to the nearest Int" $ withFreshDB \db ->
        -- The `DecodeNonNullableValue Int` instance reads the column as a `Number` and
        -- applies `round`, so fractional SQLite REAL values must come back as
        -- the nearest integer (ties rounding towards positive infinity).
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE measures (label TEXT, value REAL) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, value :: Number) @() db "INSERT INTO measures VALUES (:label, :value);"
              _ <- run insert { label: "down", value: 1.4 }
              _ <- run insert { label: "up", value: 1.6 }
              _ <- run insert { label: "half", value: 2.5 }
              select <- prepare @() @(label :: String, value :: Int) db "SELECT label, value FROM measures ORDER BY label;"
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "down", value: 1 }
            , { label: "half", value: 3 }
            , { label: "up", value: 2 }
            ]

    describe "decoding Boolean" do
      it "round-trips a Boolean through an INT column" $ withFreshDB \db ->
        -- `EncodeNonNullableValue Boolean` stores false/true as 0/1 and
        -- `DecodeNonNullableValue Boolean` reads the column as a `Number`, treating 0 as
        -- false and any other value as true.
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE flags (label TEXT, flag INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, flag :: Boolean) @() db "INSERT INTO flags VALUES (:label, :flag);"
              _ <- run insert { label: "on", flag: true }
              _ <- run insert { label: "off", flag: false }
              select <- prepare @() @(label :: String, flag :: Boolean) db "SELECT label, flag FROM flags ORDER BY label;"
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "off", flag: false }
            , { label: "on", flag: true }
            ]

      it "decodes a non-zero INT column as true" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE flags (label TEXT, flag INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, flag :: Int) @() db "INSERT INTO flags VALUES (:label, :flag);"
              _ <- run insert { label: "two", flag: 2 }
              _ <- run insert { label: "zero", flag: 0 }
              select <- prepare @() @(label :: String, flag :: Boolean) db "SELECT label, flag FROM flags ORDER BY label;"
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "two", flag: true }
            , { label: "zero", flag: false }
            ]

    describe "Char fields" do
      it "round-trips a Char through a TEXT column" $ withFreshDB \db ->
        -- `EncodeNonNullableValue Char` stores the character as a single-character TEXT
        -- and `DecodeNonNullableValue Char` reads it back via the first code unit, so a
        -- bound `Char` param must decode to the same `Char`.
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE grades (label TEXT, grade TEXT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, grade :: Char) @() db "INSERT INTO grades VALUES (:label, :grade);"
              _ <- run insert { label: "alice", grade: 'A' }
              _ <- run insert { label: "bob", grade: 'B' }
              select <- prepare @() @(label :: String, grade :: Char) db "SELECT label, grade FROM grades ORDER BY label;"
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "alice", grade: 'A' }
            , { label: "bob", grade: 'B' }
            ]

      it "decodes only the first character of a multi-character TEXT column" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE grades (label TEXT, grade TEXT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, grade :: String) @() db "INSERT INTO grades VALUES (:label, :grade);"
              _ <- run insert { label: "alice", grade: "ABC" }
              select <- prepare @() @(label :: String, grade :: Char) db "SELECT label, grade FROM grades;"
              get select {}
          )
          \row -> row `shouldEqual` Just { label: "alice", grade: 'A' }

      it "fails to decode an empty TEXT column as Char" $ withFreshDB \db -> do
        result <- runExceptT do
          create <- prepare @() @() db "CREATE TABLE grades (label TEXT, grade TEXT) STRICT;"
          _ <- run create {}
          insert <- prepare @(label :: String, grade :: String) @() db "INSERT INTO grades VALUES (:label, :grade);"
          _ <- run insert { label: "alice", grade: "" }
          select <- prepare @() @(label :: String, grade :: Char) db "SELECT label, grade FROM grades;"
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
              create <- prepare @() @() db "CREATE TABLE opt (label TEXT, val INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, val :: Maybe Int) @() db "INSERT INTO opt VALUES (:label, :val);"
              _ <- run insert { label: "present", val: Just 42 }
              _ <- run insert { label: "absent", val: Nothing }
              select <- prepare @() @(label :: String, val :: Maybe Number) db "SELECT label, val FROM opt ORDER BY label;"
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { label: "absent", val: Nothing }
            , { label: "present", val: Just 42.0 }
            ]

      it "decodes a nullable TEXT column as Maybe String" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE notes (id INT, note TEXT) STRICT;"
              _ <- run create {}
              insert <- prepare @(id :: Int, note :: Maybe String) @() db "INSERT INTO notes VALUES (:id, :note);"
              _ <- run insert { id: 1, note: Just "hello" }
              _ <- run insert { id: 2, note: Nothing }
              select <- prepare @() @(id :: Number, note :: Maybe String) db "SELECT id, note FROM notes ORDER BY id;"
              all select {}
          )
          \rows -> rows `shouldEqual`
            [ { id: 1.0, note: Just "hello" }
            , { id: 2.0, note: Nothing }
            ]

      it "decodes a Maybe column through get" $ withFreshDB \db ->
        shouldSucceed
          ( do
              create <- prepare @() @() db "CREATE TABLE opt (label TEXT, val INT) STRICT;"
              _ <- run create {}
              insert <- prepare @(label :: String, val :: Maybe Int) @() db "INSERT INTO opt VALUES (:label, :val);"
              _ <- run insert { label: "absent", val: Nothing }
              _ <- run insert { label: "present", val: Just 7 }
              select <- prepare @() @(label :: String, val :: Maybe Number) db "SELECT label, val FROM opt WHERE label = 'absent';"
              get select {}
          )
          \row -> row `shouldEqual` Just { label: "absent", val: (Nothing :: Maybe Number) }

    describe "close" do
      it "closes the database without error" $ withFreshDB \db -> do
        closeResult <- runExceptT $ close db
        case closeResult of
          Left err -> fail $ "close failed: " <> show err
          Right _ -> pure unit

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

