module CString
    ( CString(..)
    ) where

newtype CString = CString (ForeignPtr CChar)

instance IsString CString where
    fromString str =
        let bytes = TE.encodeUtf8 $ T.pack (str <> "\0")
            (fptr, off, len) = BS.toForeignPtr bytes
        in
        CString (plusForeignPtr fptr off)
