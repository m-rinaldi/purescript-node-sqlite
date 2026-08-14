module SQLite.Encoding
  ( SQLEncodedRecord
  , SQLValue(..)
  , class Encode
  , class EncodeNonNull
  , class EncodeSQLField
  , class EncodeSQLFields
  , encode
  , encodeField
  , encodeFields
  , encodedNonNull
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.String.CodeUnits (singleton)
import Data.Symbol (class IsSymbol)
import Foreign (Foreign, unsafeToForeign)
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

{-
SQLite type | JS type accepted
------------------------------
NULL	      | null
INTEGER	    | number or bigint
REAL	      | number
TEXT	      | string
BLOB	      | TypedArray or DataView
-}

foreign import data SQLValue :: Type

class EncodeNonNull (v :: Type) where
  encodedNonNull :: v -> SQLValue

instance EncodeNonNull Int where
  encodedNonNull = unsafeCoerce

instance EncodeNonNull Boolean where
  encodedNonNull false = unsafeCoerce 0
  encodedNonNull true = unsafeCoerce 1

instance EncodeNonNull Number where
  encodedNonNull = unsafeCoerce

instance EncodeNonNull String where
  encodedNonNull = unsafeCoerce

instance EncodeNonNull Char where
  encodedNonNull = singleton >>> unsafeCoerce

class EncodeSQLField (v :: Type) where
  encodeField :: v -> SQLValue

-- Maybe only accepts non-null inner types (the EncodeNonNull constraint), so no nested Maybe.
-- A single generic Maybe instance is preferred over per-type instances (Maybe Int, Maybe String, ...):
-- The reason is that NULL is orthogonal to type affinity, so it behaves uniformly for every inner type — the
-- Nothing -> null / Just v -> encode v logic is identical regardless of v, and is written once here rather than duplicated (and kept in sync) per primitive.
instance EncodeNonNull v => EncodeSQLField (Maybe v) where
  encodeField Nothing = unsafeCoerce null
  encodeField (Just v) = encodedNonNull v
else
-- primitives go through EncodeNonNull
instance EncodeNonNull v => EncodeSQLField v where
  encodeField = encodedNonNull

class EncodeSQLFields (rl :: RowList Type) (r :: Row Type) (encoded :: Row Type) | rl -> r encoded where
  encodeFields :: Proxy rl -> Record r -> Builder {} { | encoded }

instance EncodeSQLFields RowList.Nil () () where
  encodeFields :: Proxy RowList.Nil -> {} -> Builder {} {}
  encodeFields _ _ = identity

instance
  ( IsSymbol name
  , EncodeSQLField ty
  , EncodeSQLFields tail tailR encodedTailR
  , Row.Lacks name tailR
  , Row.Lacks name encodedTailR
  , Row.Cons name ty tailR r
  , Row.Cons name SQLValue encodedTailR encodedR
  ) =>
  EncodeSQLFields (RowList.Cons name ty tail) r encodedR where
  encodeFields :: Proxy (RowList.Cons name ty tail) -> Record r -> Builder {} { | encodedR }
  encodeFields _ obj =
    let
      (value :: ty) = Record.get (Proxy @name) obj
      (encodedValue :: SQLValue) = encodeField value
      (buildStep :: Builder { | encodedTailR } { | encodedR }) = Builder.insert (Proxy @name) encodedValue
      (objTail :: Record tailR) = Record.delete (Proxy @name) obj
      buildTail = encodeFields (Proxy @tail) objTail
    in
      buildTail >>> buildStep

newtype SQLEncodedRecord = SQLEncodedRecord Foreign

class Encode (v :: Type) where
  encode :: v -> SQLEncodedRecord

instance
  ( RowList.RowToList r rl
  , EncodeSQLFields rl r er
  ) =>
  Encode (Record r) where
  encode :: Record r -> SQLEncodedRecord
  encode obj =
    let
      builder = encodeFields (Proxy @rl) obj
      built = Builder.build builder {}
    in
      SQLEncodedRecord $ unsafeToForeign built