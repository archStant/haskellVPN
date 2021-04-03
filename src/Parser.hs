module Parser (Parser, repeatP, digit, manyTill, many1Till, consume, char,eof, parse, string) where
import Data.Functor.Identity(Identity(Identity), runIdentity)
import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad (guard, liftM2)
import Control.Monad.State (State,StateT(StateT), runStateT)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)

-- newtype ExceptT e m a = ExceptT (m (Either e a))
-- newtype StateT s (m :: * -> *) a = StateT (s -> m (a, s))
-- type State s = StateT s Identity
newtype Parser a = Parser { runParser :: ExceptT String (State String) a }
instance Functor Parser where
  fmap f (Parser p) = Parser $ f <$> p

instance Applicative Parser where
  pure a = Parser $ pure a
  Parser f <*> Parser a = Parser $ f <*> a

instance Alternative Parser where
  empty = Parser empty
  Parser a <|> Parser b = Parser (a <|> b)

instance Monad Parser where
  Parser m >>= f = Parser (m >>= runParser . f)

throwP :: String -> Parser a
throwP message = Parser $ throwE message

parse :: Parser a -> String -> (Either String a, String)
parse p = runIdentity . (runStateT . runExceptT . runParser) p

getState :: Parser String
getState = Parser $ ExceptT $ StateT (\state -> Identity (Right state, state))

setState :: String -> Parser ()
setState state = Parser $ ExceptT $ StateT $ \_ ->
  Identity (Right (()), state)

peek :: Parser (Maybe Char)
peek = headMaybe <$> getState
  where
    headMaybe []    = Nothing
    headMaybe (h:_) = Just h

consume :: Parser Char
consume = do
  s <- getState
  case s of
    [] -> empty
    (c:cs) -> setState cs >> return c

eof :: Parser ()
eof =  (guard . (Nothing==)) =<< peek

char :: Char -> Parser Char
char c0 = do
  mc1 <- peek
  c1  <- maybe empty return mc1
  guard (c0 == c1)
  _   <- consume
  return c1

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  liftM2 (:) (char c) (string cs)

repeatP :: Integral i => i -> Parser a -> Parser [a]
repeatP 0 _ = return []
repeatP i p = liftM2 (:) p (repeatP (i-1) p)

digit :: Integral i => Parser i
digit = char2digit =<< digitChar
  where
    digitChar = char '0' <|> char '1'
              <|> char '2' <|> char '3'
              <|> char '4' <|> char '5'
              <|> char '6' <|> char '7'
              <|> char '8' <|> char '9'
    char2digit '0' = return 0
    char2digit '1' = return 1
    char2digit '2' = return 2
    char2digit '3' = return 3
    char2digit '4' = return 4
    char2digit '5' = return 5
    char2digit '6' = return 6
    char2digit '7' = return 7
    char2digit '8' = return 8
    char2digit '9' = return 9
    char2digit nonchar = throwP ("Not a digit: " ++ show nonchar)

retry :: Parser a -> Parser a -> Parser a
retry ifP elseP = do
  s0 <- getState
  ifP <|> (setState s0 >> elseP)

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill manyParser tillParser = manyTillAux
  where
    manyTillAux = (tillParser >> return []) `retry` (liftM2 (:) manyParser manyTillAux)

many1Till :: Parser a -> Parser b -> Parser [a]
many1Till manyParser tillParser = do
  c  <- manyParser
  cs <- manyTill manyParser tillParser
  return (c:cs)
