-- tcp-server.hs
import Network
import System.IO
import System.Process
import Control.Concurrent

main :: IO ()
main = withSocketsDo $ do
  sock <- Network.listenOn $ PortNumber 3001
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
