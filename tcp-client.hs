-- tcp-client.hs

import Network
import System.IO

main :: IO ()
main = withSocketsDo $ do
  handle <- connectTo "127.0.0.1" (PortNumber 3001)
  -- hPutStr handle "Hello, world!"
  -- newCmd :: String
  -- newCmd <- readLn
  newCmd <- getLine
  hPutStr handle newCmd
  hClose handle
