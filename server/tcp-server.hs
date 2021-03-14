-- tcp-server.hs
import Network
import System.IO
import System.Process

main :: IO ()
main = withSocketsDo $ do
  sock <- Network.listenOn $ PortNumber 3001
  putStrLn "Begynder server"
  handleConnections sock


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
