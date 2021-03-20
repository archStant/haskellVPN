-- tcp-client.hs
import qualified Network as N -- To get flycheck to shut the fuck up about PortNumber
import Network            (Socket, withSocketsDo, listenOn, accept)
import System.IO          (Handle, hPutStr, hPutChar, hGetLine, hGetChar, hClose)
import System.Process     (proc, StdStream(CreatePipe), createProcess, std_in, std_out)
import Control.Concurrent (forkIO)
import Numeric            (showHex)
import qualified Numeric as N
import qualified Data.Char as DC
import Data.Hex (unhex)
import Control.Exception (try, IOException)


main :: IO ()
main = withSocketsDo $ do
  serverHandle <- N.connectTo "localhost" (N.PortNumber 3001)
  putStrLn "Roald"
  sock <- listenOn $ N.PortNumber 3002
  putStrLn "Begynder server"
  handleConnections serverHandle sock


handleConnections :: Handle -> Socket -> IO ()
handleConnections serverHandle sock = do
  -- Accept connection from client
  (handle, host, port) <- accept sock
  putStrLn "forbindelse jaaa"
  forkIO $ shovel handle serverHandle
  forkIO $ shovel serverHandle handle
  -- vent paa begge threads er lukkede
  return $ forever ()
  hClose handle
  handleConnections serverHandle sock

forever () = forever ()

shovel :: Handle -> Handle -> IO ()
shovel inhandle outhandle = do
  eOut <- try $ hGetChar inhandle ::  IO (Either IOException Char)
  case eOut of
    Right c -> do
      -- haandter lukket forbindelse
      hPutChar outhandle c
      shovel inhandle outhandle
    Left _ -> do
      putStrLn ("Lukker forbindelse " ++ (show inhandle))
      hClose outhandle
      return ()


-- main :: IO ()
-- main = N.withSocketsDo $ do
--   -- putStrLn "Address to connect to:"
--   -- serverIP <- getLine
--   -- handle <- connectTo serverIP (PortNumber 3001)
--   handle <- N.connectTo "127.0.0.1" (N.PortNumber 3001)
--   putStrLn "Connected!"
--   hPutStr handle ("initSRProtocol\n")
--   doStuff handle
--   hClose handle

-- doStuff :: Handle -> IO ()
-- doStuff handle = do
--   -- Få en linie fra brugeren
--   newCmd <- getLine
--   putStrLn "c1"
--   case newCmd of
--     "exit" -> do
--       hPutStr handle ("exitSRProtocol\n")
--       putStrLn "Closing..."
--     _ -> do
--       -- Send den til serveren
--       hPutStr handle (newCmd ++ "\n")
--       putStrLn "c2"
--       -- Få en linie fra serveren
--       output <- hGetLine handle
--       putStrLn "c3"
--       -- Send den til stdout
--       -- putStrLn output
--       putStrLn "c4"
--       -- Gør det igen
--       doStuff handle
