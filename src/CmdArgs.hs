module CmdArgs (Args(mode, port, hostname), Mode(Server,Client), getArgs) where
import Data.List (foldl')
import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import Network (HostName, PortNumber)
import qualified Options.Applicative as OA
import Options.Applicative ( ParserInfo
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
import Parser (Parser, digit, manyTill, many1Till, consume, char,eof, parse)

data Mode = Server | Client deriving (Show, Read)
data Args    =
  Args { mode :: Mode
       , port :: PortNumber
       , hostname :: (HostName, PortNumber)
       } deriving (Show)

-- should parse ports of more/less digits than 4
portParser :: Parser PortNumber
portParser = do
  digits <- many1Till digit eof
  eof
  return $ foldl' (\acc (d,e) -> acc+d*10^e) 0 $ zip digits [length digits-1, length digits-2..0]

hostParser :: Parser (HostName, PortNumber)
hostParser = do
    targetIP   <- manyTill consume $ char ':'
    _          <- char ':'
    targetPort <- portParser
    eof
    return (targetIP, targetPort)

serverArg :: OA.Parser Mode
serverArg = flag' Server
  (long "server"
  <> short 's'
  <> help "Run as a VPN server"
  )

clientArg :: OA.Parser Mode
clientArg = flag' Client
  ( long "client"
  <> short 'c'
  <> help "Runs as a VPN client"
  )

modeArg :: OA.Parser Mode
modeArg = serverArg
            <|> clientArg
            <|> option auto (value Client) -- default

portArg :: OA.Parser PortNumber
portArg = option (eitherReader $ fst . parse portParser)
  ( long "port"
    <> short 'p'
    <> help "Port to listen on"
    <> showDefault
    <> value 3001
    <> metavar "PORT"
  )

hostArg :: OA.Parser (HostName, PortNumber)
hostArg = option (eitherReader $ (fst . parse hostParser))
  ( long "host"
    <> short 'H'
    <> help "Do it right"
    <> showDefault
    <> value ("localhost", 3000)
    <> metavar "HOST:PORT"
  )

parseArgs :: OA.Parser Args
parseArgs = Args <$> modeArg <*> portArg <*> hostArg

progArgs :: ParserInfo Args
progArgs = info (helper <*> parseArgs)
           ( fullDesc
             <> progDesc "VPN server and client"
             <> header "hasVPN - a VPN written in Haskell"
      )

getArgs :: IO Args
getArgs = execParser progArgs
