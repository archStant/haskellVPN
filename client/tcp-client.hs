-- tcp-client.hs

import Network
import System.IO


main :: IO ()
main = withSocketsDo $ do
  -- putStrLn "Address to connect to:"
  -- serverIP <- getLine
  -- handle <- connectTo serverIP (PortNumber 3001)
  handle <- connectTo "127.0.0.1" (PortNumber 3001)
  putStrLn "Connected!"
  newCmd <- getLine
  -- let newCmd = "test"
  hPutStr handle newCmd
  -- response <- hGetLine handle
  -- putStrLn response
  hClose handle
