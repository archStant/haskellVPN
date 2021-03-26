import CmdArgs (getArgs,Args(port, mode, hostname), Mode(Server,Client))
import Server (server)
import Client (client)


main :: IO ()
main = do
  args <- getArgs
  case mode args of
    Server -> server $ port args
    Client -> client (port args) (hostname args)
