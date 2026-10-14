module Node.SQLite.Encoding.EncodedValue
  ( EncodedValue
  , null
  , encodeInt
  , encodeReal
  , encodeText
  ) where

import Prelude

import Data.Nullable as Nullable
import Foreign (Foreign, unsafeToForeign)

{-
SQLite type | JS type accepted
------------------------------
NULL	      | null
INTEGER	    | number or bigint
REAL	      | number
TEXT	      | string
BLOB	      | TypedArray or DataView
-}

-- | A JS value accepted by node:sqlite as a bound parameter.
-- | Opaque: the constructor is not exported, so values can only be built with
-- | the smart constructors below, one per SQLite storage class.
newtype EncodedValue = EncodedValue Foreign

-- | SQL `NULL`, bound as JS `null`.
null :: EncodedValue
null = unsafeToForeign Nullable.null # EncodedValue

-- | SQL `INTEGER`, bound as a JS number.
encodeInt :: Int -> EncodedValue
encodeInt = unsafeToForeign >>> EncodedValue

-- | SQL `REAL`, bound as a JS number.
encodeReal :: Number -> EncodedValue
encodeReal = unsafeToForeign >>> EncodedValue

-- | SQL `TEXT`, bound as a JS string.
encodeText :: String -> EncodedValue
encodeText = unsafeToForeign >>> EncodedValue
