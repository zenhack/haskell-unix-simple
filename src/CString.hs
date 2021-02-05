module CString
    ( CString
    , CStr(..)
    , useCStr
    , fromBuilder
    , toBuilder
    ) where

import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as LBS
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Zhp

-- | wrapper around a nul-terminated C style string; the pointer points to the
-- beginning of the string.
--
-- Users of the library will mostly not use this directly, instead using CString.
newtype CStr = CStr (Ptr CChar)

-- | A string for passing to C api functions. The C-compatible form is computed
-- lazily; it will not be forced until the string is passed to a C API function.
-- Internally, this is stored as a 'BB.Builder' with no trailing nul, so
-- performance characteristics are mostly the same, only requiring a copy when
-- first passing the string to an API function.
data CString = CString
    { bytes :: BB.Builder
    , fptr  :: ForeignPtr CChar
    }

instance Semigroup CString where
    x <> y = fromBuilder (toBuilder x <> toBuilder y)

instance Monoid CString where
    mempty = fromBuilder mempty

-- | Convert a 'BB.Builder' to a CString. The builder should not have a nul
-- terminator; it will be added.
fromBuilder :: BB.Builder -> CString
fromBuilder builder = CString
    { bytes = builder
    , fptr =
        let bs = LBS.toStrict $ BB.toLazyByteString (builder <> BB.word8 0)
            (ptr, off, _len) = BS.toForeignPtr bs
        in
        plusForeignPtr ptr off
    }

-- | Extract a bytestring builder for the string. Does not include the nul
-- terminator. O(1).
toBuilder :: CString -> BB.Builder
toBuilder = bytes

-- | Use the raw pointer underlying the 'CString'.
useCStr :: CString -> (CStr -> IO a) -> IO a
useCStr str use =
    withForeignPtr (fptr str) $ \ptr -> use (CStr ptr)

instance IsString CString where
    fromString = fromBuilder . BB.stringUtf8
