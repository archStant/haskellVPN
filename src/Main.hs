import CmdArgs (getArgs, Args(port, mode), Mode(Server,Client))
import Server (server)
import Client (client)


main :: IO ()
main = do
  args <- getArgs
  case mode args of
    Server -> server $ port args
    Client -> client $ port args
