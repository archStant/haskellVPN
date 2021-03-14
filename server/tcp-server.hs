-- tcp-server.hs
import Network
import System.IO
import qualified System.Process

main :: IO ()
main = withSocketsDo $ do
  sock <- Network.listenOn $ PortNumber 3001
  putStrLn "Begynder server"
  handleConnections sock

  (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <-
    createProcess (proc "ls" []){ std_out = CreatePipe }


handleConnections :: Socket -> IO ()
handleConnections sock = do
  (handle, host, port) <- accept sock
  output <- hGetLine handle
  putStrLn output
  res <- System.Process.readProcess output [] ""
  -- hPutStr handle (System.Process.readProcess output [] "")
  putStrLn res
  -- hPutStr handle "Ok"
  handleConnections sock
