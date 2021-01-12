module Unix.Errors
    ( retryEINTR
    , throwIfErrno
    ) where

import Zhp

import Control.Exception (throwIO)
import Foreign.C.Error

throwIfErrno :: IO (Either Errno a) -> IO a
throwIfErrno io = do
    r <- io
    case r of
        Left e  -> throwIO $ errnoToIOError "" e Nothing Nothing
        Right v -> pure v

retryEINTR :: IO (Either Errno a) -> IO (Either Errno a)
retryEINTR io = do
    r <- io
    case r of
        Left e | e == eINTR -> retryEINTR io
        _                   -> pure r
