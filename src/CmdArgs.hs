module CmdArgs (Args(mode, port, hostname), Mode(Server,Client), getArgs) where
import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Network (HostName, PortNumber)
import Options.Applicative ( Parser
                           , ParserInfo
                           , flag'
                           , short
                           , help
                           , long
                           , option
                           , showDefault
                           , value
                           , auto
                           , metavar
                           , eitherReader
                           , info
                           , helper
                           , fullDesc
                           , progDesc
                           , header
                           , execParser
                           )

data Mode = Server | Client deriving (Show, Read)
data Args    =
  Args { mode :: Mode
       , port :: PortNumber
       , hostname :: (HostName, PortNumber)
       } deriving (Show)

parseSingle :: Char -> String -> Either String String
parseSingle c "" = Left ("Missing " ++ [c])
parseSingle c (x:xs)
  | c == x    = Right xs
  | otherwise = Left ("Missing " ++ [c])

headEither :: [a] -> Either String a
headEither []    = Left "Dummernik"
headEither str@(_:_) = Right $ head str

parseTargetAddress :: String -> Either String (HostName, PortNumber)
parseTargetAddress str = do
    (targetIP, str0) <- Right $ break (\c -> c == ':') str
    str1 <- parseSingle ':' str0
    (targetPort, "") <- headEither $ reads str1
    return (targetIP, targetPort)


parseServer :: Parser Mode
parseServer = flag' Server
  (long "server"
  <> short 's'
  <> help "Run as a VPN server"
  )

parseClient :: Parser Mode
parseClient = flag' Client
  ( long "client"
  <> short 'c'
  <> help "Runs as a VPN client"
  )

parseMode :: Parser Mode
parseMode = parseServer
            <|> parseClient
            <|> option auto (value Client) -- default

parsePort :: Parser PortNumber
parsePort = option auto
  ( long "port"
    <> short 'p'
    <> help "Port to listen on"
    <> showDefault
    <> value 3001
    <> metavar "PORT"
  )

parseHost :: Parser (HostName, PortNumber)
parseHost = option (eitherReader parseTargetAddress)
  ( long "host"
    <> short 'H'
    <> help "Do it right"
    <> showDefault
    <> value ("localhost", 3000)
    <> metavar "HOST:PORT"
  )

parseArgs :: Parser Args
parseArgs = Args <$> parseMode <*> parsePort <*> parseHost

progArgs :: ParserInfo Args
progArgs = info (helper <*> parseArgs)
           ( fullDesc
             <> progDesc "VPN server and client"
             <> header "hasVPN - a VPN written in Haskell"
      )

getArgs :: IO Args
getArgs = execParser progArgs
