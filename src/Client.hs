module Client (client) where
import Control.Concurrent (forkIO)
import Control.Exception (try, IOException)
import Network (PortNumber, PortID(PortNumber), HostName, connectTo)
import Network (Socket, withSocketsDo, listenOn, accept)
import System.IO (Handle, hPutChar, hGetChar, hClose)


client :: PortNumber -> (HostName, PortNumber) -> IO ()
client inport (hostname, outport) = withSocketsDo $ do
  serverHandle <- connectTo hostname (PortNumber outport)
  sock <- listenOn $ PortNumber inport
  putStrLn "Begynder client"
  handleConnections serverHandle sock

handleConnections :: Handle -> Socket -> IO ()
handleConnections serverHandle sock = do
  -- Accept connection from client
  (handle, _, _) <- accept sock
  putStrLn (show handle ++ " " ++ show serverHandle)
  _ <- forkIO $ shovel serverHandle handle
  _ <- forkIO $ shovel handle serverHandle
  -- vent paa begge threads er lukkede
  handleConnections serverHandle sock

shovel :: Handle -> Handle -> IO ()
shovel inhandle outhandle = do
  eOut <- try $ hGetChar inhandle ::  IO (Either IOException Char)
  case eOut of
    Right c -> do
      -- haandter lukket forbindelse
      putStrLn [c]
      hPutChar outhandle c
      shovel inhandle outhandle
    Left _ -> do
      putStrLn ("Lukker forbindelse " ++ (show inhandle))
      hClose outhandle
      return ()
