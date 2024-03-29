A straightforward wrapping of unix APIs for Haskell.

I wrote this package because state of posix API support in Haskell is
somewhat unsatisfactory in a few ways:

* There are many syscalls not covered by the `unix` package
* The `unix` package needlessly renames syscalls, making them harder to
  find for people familiar with the posix API. For example, `lstat` has
  been renamed to `getSymbolicLinkStatus`
* There are a large number of packages that fill in various holes, but
  you have to scrape them together.

The goal of this package then, is to provide a one-stop-shop for unix
API bindings, with a straightforward mapping to the underlying C API.
At the time of writing there are many syscalls missing, but in theory
most things belong here. Patches welcome; see the section below re:
how to add a syscall.

# General Conventions

* Function names are the same as the underlying system calls, so there
  is no guesswork.
* Wrappers automatically retry on `EINTR` where appropriate (this is
  safe for most system calls, but not all, e.g. `close` on Linux).
* The basic versions of the syscalls return `IO (Either Errno a)`,
  rather than raising an exception.
  * Each system call also has a variant suffixed with `Exn` that throws
    exceptions.
  * We define a type alias `type EIO a = IO (Either Errno a)` for
    convenience.
* For flags, we add newtype wrappers, and they can be combined with
  their `SemiGroup` instances, e.g.
  * `open "hello.txt" (o_CREAT <> o_RDWR) 0o600`
* Flags are named the same as the C constant, but with the first
  character lower-cased.
* For functions that take a buffer, we instead accept a `ByteString`
  when the buffer is read, or return one when it is written, e.g.
  * `read :: Fd -> Int -> EIO BS.ByteString`
  * `write :: Fd -> BS.ByteString -> EIO CSsize`
* We also provide variants suffixed with `Buf`, that take a pointer and
  size:
  * `readBuf :: Fd -> Ptr Word8 -> CSize -> EIO CSsize`
* For functions that take an array of `struct iovec` as an input,
  we provide several variants, e.g. for `writev` we provide:
  * `writev :: Fd -> [BS.ByteString] -> EIO CSize`
  * `writevBuf :: Fd -> Ptr CIOVec -> CInt -> EIO CSsize`
  * `writevVec :: Fd -> SMV.IOVector CIOVec -> EIO CSize`
  (How to support readv is still an open design question).
* We provide a `CString` type for functions which accept strings as
  arguments. This type is an instance of `IsString`, so you can use
  string literals if you enable `OverloadedStrings`, or use the
  `fromString` function to convert. The conversion uses utf-8 encoding.
* For some calls we also add obvious convenience helpers, e.g.
  `readFull` and `writeFull`, which wrap `read` and `write` and handle
  short reads and writes for the caller.

# Contributing

## Adding a syscall

To add a new system call:

* Add the appropriate declaration to `Unix.C`, following conventions
  established by the examples in that file.
* Add a wrapper following the general conventions in `Unix`.
