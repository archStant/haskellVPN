-- tcp-server.hs
import qualified Network as N -- To get flycheck to shut the fuck up about PortNumber
import Network            (Socket, withSocketsDo, listenOn, accept)
import System.IO          (Handle, hPutStr, hGetLine, hGetChar, hClose)
import System.Process     (proc, StdStream(CreatePipe), createProcess, std_in, std_out)
import Control.Concurrent (forkIO)
import Numeric            (showHex)
import qualified Numeric as N
import qualified Data.Char as DC
import Data.Hex (unhex)
import Control.Exception (try, IOException)

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn $ N.PortNumber 3001
  putStrLn "Begynder server"
  handleConnections sock


handleConnections :: Socket -> IO ()
handleConnections sock = do
  -- Accept connection from client
  (handle, host, port) <- accept sock
  forkIO $ sendHex handle
  shovel handle
  hClose handle
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
  if (i <= 15)
    then putStr "0"
    else return ()
  putStr $ showHex i ""


shovel :: Handle -> IO ()
shovel handle = do
  eOut <- try $ hGetChar handle ::  IO (Either IOException Char)
  case eOut of
    Right c -> do
      putHexChar c
      shovel handle
    Left _ -> do
      putStrLn ("Lukker forbindelse " ++ (show handle))
      return ()
