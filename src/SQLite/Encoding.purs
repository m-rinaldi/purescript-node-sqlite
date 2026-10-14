module Node.SQLite.Encoding
  ( EncodedRecord
  , class Encode
  , encode
  , class EncodeFields
  , encodeFields
  , class EncodeValue
  , encodeValue
  , class EncodeNonNullableValue
  , encodeNonNullableValue
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton)
import Data.Symbol (class IsSymbol)
import Foreign (Foreign, unsafeToForeign)
import Node.SQLite.Encoding.EncodedValue (EncodedValue, encodeInt, encodeReal, encodeText, null)
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

-- | Extension point. Encodes a PureScript type as a single SQL value that is
-- | never `NULL`. Add an instance to make a type usable as a column, directly
-- | or wrapped in `Maybe`.
class EncodeNonNullableValue (v :: Type) where
  encodeNonNullableValue :: v -> EncodedValue

instance EncodeNonNullableValue Int where
  encodeNonNullableValue = encodeInt

instance EncodeNonNullableValue Boolean where
  encodeNonNullableValue false = encodeInt 0
  encodeNonNullableValue true = encodeInt 1

instance EncodeNonNullableValue Number where
  encodeNonNullableValue = encodeReal

instance EncodeNonNullableValue String where
  encodeNonNullableValue = encodeText

instance EncodeNonNullableValue Char where
  encodeNonNullableValue = singleton >>> encodeText

-- | One SQL value, `NULL` allowed. Not an extension point: the fallback
-- | instance below overlaps with any instance written elsewhere, so nullability
-- | is expressed by wrapping in `Maybe`.
class EncodeValue (v :: Type) where
  encodeValue :: v -> EncodedValue

-- Maybe only accepts non-nullable inner types (the EncodeNonNullableValue constraint), so no nested Maybe.
-- A single generic Maybe instance is preferred over per-type instances (Maybe Int, Maybe String, ...):
-- The reason is that NULL is orthogonal to type affinity, so it behaves uniformly for every inner type — the
-- Nothing -> null / Just v -> encode v logic is identical regardless of v, and is written once here rather than duplicated (and kept in sync) per primitive.
instance EncodeNonNullableValue v => EncodeValue (Maybe v) where
  encodeValue Nothing = null
  encodeValue (Just v) = encodeNonNullableValue v
else
-- primitives go through EncodeNonNullableValue
instance EncodeNonNullableValue v => EncodeValue v where
  encodeValue = encodeNonNullableValue

-- | Internal. `RowList` traversal behind the `Encode (Record r)` instance.
class EncodeFields (rl :: RowList Type) (r :: Row Type) (encoded :: Row Type) | rl -> r encoded where
  encodeFields :: Proxy rl -> Record r -> Builder {} { | encoded }

instance EncodeFields RowList.Nil () () where
  encodeFields :: Proxy RowList.Nil -> {} -> Builder {} {}
  encodeFields _ _ = identity

instance
  ( IsSymbol name
  , EncodeValue ty
  , EncodeFields tail tailR encodedTailR
  , Row.Lacks name tailR
  , Row.Lacks name encodedTailR
  , Row.Cons name ty tailR r
  , Row.Cons name EncodedValue encodedTailR encodedR
  ) =>
  EncodeFields (RowList.Cons name ty tail) r encodedR where
  encodeFields :: Proxy (RowList.Cons name ty tail) -> Record r -> Builder {} { | encodedR }
  encodeFields _ obj =
    let
      (value :: ty) = Record.get (Proxy @name) obj
      (encodedValue :: EncodedValue) = encodeValue value
      (buildStep :: Builder { | encodedTailR } { | encodedR }) = Builder.insert (Proxy @name) encodedValue
      (objTail :: Record tailR) = Record.delete (Proxy @name) obj
      buildTail = encodeFields (Proxy @tail) objTail
    in
      buildTail >>> buildStep

newtype EncodedRecord = EncodedRecord Foreign

class Encode (v :: Type) where
  encode :: v -> EncodedRecord

instance
  ( RowList.RowToList r rl
  , EncodeFields rl r er
  ) =>
  Encode (Record r) where
  encode :: Record r -> EncodedRecord
  encode obj =
    let
      builder = encodeFields (Proxy @rl) obj
      built = Builder.build builder {}
    in
      EncodedRecord $ unsafeToForeign built