module SQLite.Decoding
  ( Error
  , class DecodeNonNull
  , class DecodeSQL
  , class DecodeSQLField
  , class DecodeSQLFields
  , decode
  , decodeField
  , decodeFields
  , decodeNonNull
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
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

type Error = NonEmptyList ForeignError

class DecodeNonNull (v :: Type) where
  decodeNonNull :: ∀ m. Monad m => Foreign -> ExceptT Error m v

instance DecodeNonNull String where
  decodeNonNull val = readString val

instance DecodeNonNull Char where
  decodeNonNull val = do
    str <- readString val
    case uncons str of
      Just { head } -> pure head
      Nothing -> throwError $ pure $ ForeignError "Expected non-empty string on decoding for Char"

instance DecodeNonNull Number where
  decodeNonNull val = readNumber val

instance DecodeNonNull Int where
  decodeNonNull val = readNumber val <#> round

class DecodeSQLField (v :: Type) where
  decodeField :: ∀ m. Monad m => Foreign -> ExceptT Error m v

instance DecodeNonNull v => DecodeSQLField (Maybe v) where
  decodeField val
    | isNull val = pure Nothing
    | otherwise = Just <$> decodeNonNull val
else instance DecodeNonNull v => DecodeSQLField v where
  decodeField = decodeNonNull

class DecodeSQLFields (rl :: RowList Type) (row :: Row Type) | rl -> row where
  decodeFields :: ∀ m. Monad m => Proxy rl -> Foreign -> ExceptT Error m (Builder {} (Record row))

instance DecodeSQLFields RowList.Nil () where
  decodeFields :: ∀ m. Monad m => Proxy RowList.Nil -> Foreign -> ExceptT Error m (Builder {} {})
  decodeFields _ _ = ExceptT $ pure $ Right identity

instance
  ( DecodeSQLFields tail tailRow
  , IsSymbol name
  , Row.Lacks name tailRow
  , Row.Cons name ty tailRow row
  , DecodeSQLField ty
  ) =>
  DecodeSQLFields (RowList.Cons name ty tail) row where
  decodeFields :: ∀ m. Monad m => Proxy (RowList.Cons name ty tail) -> Foreign -> ExceptT Error m (Builder {} (Record row))
  decodeFields _ obj = do
    let name = reflectSymbol (Proxy @name)
    foreignField <- readProp name obj
    val <- decodeField foreignField
    restBuilder <- decodeFields (Proxy @tail) obj
    pure $ restBuilder >>> Builder.insert (Proxy @name) val

class DecodeSQL (t :: Type) where
  decode :: ∀ m. Monad m => Foreign -> ExceptT Error m t

instance
  ( RowToList r rl
  , DecodeSQLFields rl r
  ) =>
  DecodeSQL (Record r) where
  decode foreignObject = do
    builder <- decodeFields (Proxy @rl) foreignObject
    pure $ Builder.build builder {}

instance
  ( RowToList r rl
  , DecodeSQLFields rl r
  ) =>
  DecodeSQL (Maybe (Record r)) where
  decode foreignValue =
    if isUndefined foreignValue then pure Nothing
    else Just <$> decode foreignValue

instance
  ( RowToList r rl
  , DecodeSQLFields rl r
  ) =>
  DecodeSQL (Array (Record r)) where
  decode foreignArray = readArray foreignArray >>= traverse decode

