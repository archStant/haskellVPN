-- tcp-server.hs
import qualified Network as N -- To get flycheck to shut the fuck up about PortNumber
import Network            (Socket, withSocketsDo, listenOn, accept)
import System.IO          (Handle, hPutStr, hGetLine, hClose)
import System.Process     (proc, StdStream(CreatePipe), createProcess, std_in, std_out)
import Control.Concurrent (forkIO)

main :: IO ()
main = withSocketsDo $ do
  sock <- listenOn $ N.PortNumber 3001
  putStrLn "Begynder server"
  handleConnections sock


handleConnections :: Socket -> IO ()
handleConnections sock = do
  -- Accept connection from client
  (handle, host, port) <- accept sock
  -- Check for start of protocol transmission
  output <- hGetLine handle
  case output of
    "initSRProtocol" -> do
      putStrLn "a"
      let cp = (proc "/bin/bash" [] )
               {std_in = CreatePipe, std_out = CreatePipe}
      putStrLn "b"
      (Just bashIn, Just bashOut, _, _) <- createProcess cp
      putStrLn "c"
      forkIO $ dumpHandle bashOut
      doStuff handle bashIn
      -- TODO : Wait på bash-process
      return ()
    _ -> return ()
  hClose handle
  handleConnections sock

dumpHandle :: Handle -> IO ()
dumpHandle handle = do
  tmp <- hGetLine handle
  putStrLn ("> " ++ tmp)
  -- putStrLn "> "
  dumpHandle handle

doStuff :: Handle -> Handle -> IO ()
doStuff handle bashIn = do
  -- Få en linie fra klienten
  putStrLn "0"
  output <- hGetLine handle
  putStrLn "1"
  case output of
    "exitSRProtocol" -> putStrLn "Closing connection"
    _ -> do
      -- Send den til stdout
      putStrLn "2"
      putStrLn output
      hPutStr bashIn (output ++ "\n")
      -- Send en newline til klienten
      hPutStr handle "\n"
      putStrLn "3"
      -- Gør det igen
      doStuff handle bashIn
