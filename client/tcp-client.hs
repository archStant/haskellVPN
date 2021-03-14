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
  hPutStr handle ("initSRProtocol\n")
  doStuff handle
  hClose handle

doStuff :: Handle -> IO ()
doStuff handle = do
  -- Få en linie fra brugeren
  newCmd <- getLine
  putStrLn "c1"
  case newCmd of
    "exit" -> do
      hPutStr handle ("exitSRProtocol\n")
      putStrLn "Closing..."
    _ -> do
      -- Send den til serveren
      hPutStr handle (newCmd ++ "\n")
      putStrLn "c2"
      -- Få en linie fra serveren
      output <- hGetLine handle
      putStrLn "c3"
      -- Send den til stdout
      -- putStrLn output
      putStrLn "c4"
      -- Gør det igen
      doStuff handle
