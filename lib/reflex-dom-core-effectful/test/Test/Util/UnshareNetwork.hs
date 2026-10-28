{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Test.Util.UnshareNetwork where

import qualified Network.Socket as Socket
#ifdef linux_HOST_OS
import System.Linux.Namespaces
import System.Posix
import System.Process
#elif defined(darwin_HOST_OS)
import Control.Exception (bracket)
import Control.Monad (unless)
import Foreign
import Foreign.C
import System.Environment (lookupEnv)
#endif

unshareNetwork :: IO ()
#ifdef linux_HOST_OS
unshareNetwork = do
  uid <- getEffectiveUserID
  unshare [User, Network]
  writeUserMappings Nothing [UserMapping 0 uid 1]
  callCommand "ip link set lo up ; ip addr"
#elif defined(darwin_HOST_OS)
unshareNetwork = do
  platform <- lookupEnv "JENGA_DEV_PLATFORM"
  unless (platform == Just "macos") $
    ioError (userError "macOS browser isolation requires JENGA_DEV_PLATFORM=macos")
  withCString loopbackProfile $ \profile -> alloca $ \errorPtr -> do
    poke errorPtr nullPtr
    result <- sandboxInit profile 0 errorPtr
    unless (result == 0) $ sandboxFailure errorPtr

sandboxFailure :: Ptr CString -> IO a
sandboxFailure errorPtr = do
  message <- peek errorPtr
  if message == nullPtr
    then ioError (userError "Unable to apply macOS browser network sandbox")
    else bracket (pure message) sandboxFreeError $ \ptr ->
      peekCString ptr >>= ioError . userError

-- Inherited by browser/server children. Darwin shares the host's loopback
-- ports; this policy restricts network access, not port namespaces.
loopbackProfile :: String
loopbackProfile = unlines
  [ "(version 1)"
  , "(allow default)"
  , "(deny network*)"
  , "(allow network-inbound network-bind (local ip \"localhost:*\"))"
  , "(allow network-outbound (remote ip \"localhost:*\"))"
  , "(allow network* (local unix-socket) (remote unix-socket))"
  ]

foreign import ccall unsafe "sandbox_init"
  sandboxInit :: CString -> CULLong -> Ptr CString -> IO CInt
foreign import ccall unsafe "sandbox_free_error"
  sandboxFreeError :: CString -> IO ()
#else
#error Unsupported browser-test isolation platform
#endif

-- Darwin shares the host port namespace. The child server must bind promptly;
-- a competing bind fails normally rather than connecting to a fixed host port.
testPort :: Socket.PortNumber -> IO Socket.PortNumber
#ifdef darwin_HOST_OS
testPort _ = bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
  Socket.close $ \socket -> do
    Socket.bind socket $ Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1))
    address <- Socket.getSocketName socket
    case address of
      Socket.SockAddrInet port _ -> pure port
      _ -> ioError (userError "Expected an IPv4 loopback test socket")
#else
testPort = pure
#endif
