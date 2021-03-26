module CmdArgs (Args(mode, port, hostname), Mode(Server,Client), getArgs) where
import qualified Options.Applicative as OA
import Control.Applicative ((<|>))
import Data.Monoid ((<>))
import Network (HostName, PortNumber)
--import Control.Exception (try)
--import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)

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


parseServer :: OA.Parser Mode
parseServer = OA.flag' Server
  (OA.long "server"
  <> OA.short 's'
  <> OA.help "Run as a VPN server"
  )

parseClient :: OA.Parser Mode
parseClient = OA.flag' Client
  ( OA.long "client"
  <> OA.short 'c'
  <> OA.help "Runs as a VPN client"
  )

parseMode :: OA.Parser Mode
parseMode = parseServer
            <|> parseClient
            <|> OA.option OA.auto (OA.value Client) -- default

parsePort :: OA.Parser PortNumber
parsePort = OA.option OA.auto
  ( OA.long "port"
    <> OA.short 'p'
    <> OA.help "Port to listen on"
    <> OA.showDefault
    <> OA.value 3001
    <> OA.metavar "PORT"
  )

parseHost :: OA.Parser (HostName, PortNumber)
parseHost = OA.option (OA.eitherReader parseTargetAddress)
  ( OA.long "host"
    <> OA.short 'H'
    <> OA.help "Do it right"
    <> OA.showDefault
    <> OA.value ("localhost", 3000)
    <> OA.metavar "HOST:PORT"
  )

parseArgs :: OA.Parser Args
parseArgs = Args <$> parseMode <*> parsePort <*> parseHost

progArgs :: OA.ParserInfo Args
progArgs = OA.info (OA.helper <*> parseArgs)
           ( OA.fullDesc
             <> OA.progDesc "VPN server and client"
             <> OA.header "hasVPN - a VPN written in Haskell"
      )

getArgs :: IO Args
getArgs = OA.execParser progArgs
