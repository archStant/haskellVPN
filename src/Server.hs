{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}

module Server (server) where
import qualified Network as N -- To get flycheck to shut the fuck up about PortNumber
import Network            (Socket, withSocketsDo, listenOn, accept)
import System.IO          (Handle, hPutStr, hGetChar, hClose)
import Control.Concurrent (forkIO)
import Numeric            (showHex)
import Data.Hex (unhex)
import Control.Exception (try, IOException)

server :: N.PortNumber -> IO ()
server port = withSocketsDo $ do
  sock <- listenOn $ N.PortNumber port
  putStrLn "Begynder server"
  handleConnections sock


handleConnections :: Socket -> IO ()
handleConnections sock = do
  -- Accept connection from client
  (handle, _, _) <- accept sock
  putStrLn "accept"
  _ <- forkIO $ sendHex handle
  shovelStdOut handle
  putStrLn "efter shovel"
  hClose handle
  putStrLn "lukket handle"
  handleConnections sock


sendHex :: Handle -> IO ()
sendHex handle = do
  line <- getLine
  -- do
  --   str  <- unhex line
  --   try $ hPutStr handle str ::  IO (Either IOException ())
  --   sendHex handle
  case unhex line of
    Just str -> do
      eOut <- try $ hPutStr handle str ::  IO (Either IOException ())
      case eOut of
        Right () -> sendHex handle
        Left _   -> return ()
    Nothing -> sendHex handle


putHexChar :: Char -> IO ()
putHexChar c = do
  let i = fromEnum c
  if i < 16
    then putStr "0"
    else return ()

  putStr $ showHex i ""


shovelStdOut :: Handle -> IO ()
shovelStdOut handle = do
  --putStrLn "i shovel"
  eOut <- try $ hGetChar handle ::  IO (Either IOException Char)
  --putStrLn "laest char"
  case eOut of
    Right c -> do
      putHexChar c
      shovelStdOut handle
    Left _ -> do
      putStrLn ("Lukker forbindelse " ++ (show handle))
      return ()
