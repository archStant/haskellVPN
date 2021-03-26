import CmdArgs (getArgs,Args(port, mode, hostname), Mode(Server,Client))
import Server (server)
import Client (client)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case mode args of
    Server -> server $ port args
    Client -> client (port args) (hostname args)
