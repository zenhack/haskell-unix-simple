module Unix.C.Errors
    ( orErrno
    ) where

import Foreign.C.Error
import Zhp

orErrno :: IO a -> IO (Either Errno a)
orErrno io = do
    resetErrno
    r <- io
    e <- getErrno
    pure $! if e /= eOK
        then Left e
        else Right r
