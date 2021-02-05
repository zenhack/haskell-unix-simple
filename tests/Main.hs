{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

-- NOTE: in the test suite we don't generally bother worry about async exception
-- safety. It is actually useful *not* to clean up temporary files in the event
-- of an exception, because it allows us to examine the state.

import Control.Monad.Fail (fail)
import System.IO
import Unix
import Zhp


main :: IO ()
main = do
    let tmpDir :: CString
        tmpDir = "test-tmp"
        pathA :: CString
        pathA = tmpDir <> "/a"
        contentsA :: IsString a => a
        contentsA = "File A"

    mkdirExn tmpDir 0o700
    fd <- openExn pathA (o_CREAT <> o_EXCL) 0o600

    size <- writeExn fd contentsA
    -- FIXME: this is returning -1, but it's not throwing an exception?
    print size

    closeExn fd
    fd <- openExn pathA o_RDONLY 0o700
    contents <- readExn fd (length (contentsA :: String))
    when (contents /= contentsA) $
        fail $ "Unexpected file contents: " <> show contents
    closeExn fd
    removeExn pathA
    rmdirExn tmpDir
