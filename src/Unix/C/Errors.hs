module Unix.C.Errors
    ( orErrno
    ) where

import Foreign.C.Error
import Zhp

orErrno :: IO a -> IO (Either Errno a)
orErrno io = do
    r <- io
    e <- getErrno
    pure $! if e == eINTR
        then Left e
        else Right r
