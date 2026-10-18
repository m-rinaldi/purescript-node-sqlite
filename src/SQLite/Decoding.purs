module Node.SQLite.Decoding
  ( Error
  , class Decode
  , decode
  , class DecodeFields
  , decodeFields
  , class DecodeValue
  , decodeValue
  , class DecodeNonNullableValue
  , decodeNonNullableValue
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), throwError)
import Data.Either (Either(..))
import Data.Int (round)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (uncons)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Foreign (Foreign, ForeignError(..), isNull, isUndefined, readArray, readNumber, readString)
import Foreign.Index (readProp)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Prim.TypeError (class Fail, Text, Above, Beside, Quote)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

type Error = NonEmptyList ForeignError

-- | Extension point. Decodes a single SQL value that is never `NULL` into a
-- | PureScript type. Add an instance to make a type usable as a column,
-- | directly or wrapped in `Maybe`.
class DecodeNonNullableValue (v :: Type) where
  decodeNonNullableValue :: ∀ m. Monad m => Foreign -> ExceptT Error m v

instance DecodeNonNullableValue String where
  decodeNonNullableValue val = readString val

instance DecodeNonNullableValue Char where
  decodeNonNullableValue val = do
    str <- readString val
    case uncons str of
      Just { head } -> pure head
      Nothing -> throwError $ pure $ ForeignError "Expected non-empty string on decoding for Char"

instance DecodeNonNullableValue Number where
  decodeNonNullableValue val = readNumber val

instance DecodeNonNullableValue Int where
  decodeNonNullableValue val = readNumber val <#> round

instance DecodeNonNullableValue Boolean where
  decodeNonNullableValue val = readNumber val <#> (_ /= 0.0)

-- | A column is either nullable (`Maybe a`) or not (`a`). `Maybe (Maybe a)`
-- | has no meaning for a single SQL value, so reject it with a clear message.
instance
  Fail
    ( Above
        (Beside (Text "Column type ") (Beside (Quote (Maybe (Maybe a))) (Text " nests Maybe twice, but a SQL value is either NULL or not.")))
        ( Above
            (Beside (Text "Use ") (Beside (Quote (Maybe a)) (Text " if the column can be NULL,")))
            (Beside (Text "or ") (Beside (Quote a) (Text " if it cannot.")))
        )
    ) =>
  DecodeNonNullableValue (Maybe a) where
  decodeNonNullableValue _ = throwError $ pure $ ForeignError "unreachable"

-- | One SQL value, `NULL` allowed. Not an extension point: the fallback
-- | instance below overlaps with any instance written elsewhere, so nullability
-- | is expressed by wrapping in `Maybe`.
class DecodeValue (v :: Type) where
  decodeValue :: ∀ m. Monad m => Foreign -> ExceptT Error m v

instance DecodeNonNullableValue v => DecodeValue (Maybe v) where
  decodeValue val
    | isNull val = pure Nothing
    | otherwise = Just <$> decodeNonNullableValue val
else instance DecodeNonNullableValue v => DecodeValue v where
  decodeValue = decodeNonNullableValue

-- | Internal. `RowList` traversal behind the `Decode` instances.
class DecodeFields (rl :: RowList Type) (row :: Row Type) | rl -> row where
  decodeFields :: ∀ m. Monad m => Proxy rl -> Foreign -> ExceptT Error m (Builder {} (Record row))

instance DecodeFields RowList.Nil () where
  decodeFields :: ∀ m. Monad m => Proxy RowList.Nil -> Foreign -> ExceptT Error m (Builder {} {})
  decodeFields _ _ = ExceptT $ pure $ Right identity

instance
  ( DecodeFields tail tailRow
  , IsSymbol name
  , Row.Lacks name tailRow
  , Row.Cons name ty tailRow row
  , DecodeValue ty
  ) =>
  DecodeFields (RowList.Cons name ty tail) row where
  decodeFields :: ∀ m. Monad m => Proxy (RowList.Cons name ty tail) -> Foreign -> ExceptT Error m (Builder {} (Record row))
  decodeFields _ obj = do
    let name = reflectSymbol (Proxy @name)
    foreignField <- readProp name obj
    val <- decodeValue foreignField
    restBuilder <- decodeFields (Proxy @tail) obj
    pure $ restBuilder >>> Builder.insert (Proxy @name) val

class Decode (t :: Type) where
  decode :: ∀ m. Monad m => Foreign -> ExceptT Error m t

instance
  ( RowToList r rl
  , DecodeFields rl r
  ) =>
  Decode (Record r) where
  decode foreignObject = do
    builder <- decodeFields (Proxy @rl) foreignObject
    pure $ Builder.build builder {}

instance
  ( RowToList r rl
  , DecodeFields rl r
  ) =>
  Decode (Maybe (Record r)) where
  decode foreignValue =
    if isUndefined foreignValue then pure Nothing
    else Just <$> decode foreignValue

instance
  ( RowToList r rl
  , DecodeFields rl r
  ) =>
  Decode (Array (Record r)) where
  decode foreignArray = readArray foreignArray >>= traverse decode

