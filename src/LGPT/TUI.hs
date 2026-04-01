module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The 
code that is included here is a good starting point, but you don't need to 
keep it if you don't want to.
-}
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import LGPT.Helpers (Parser, prompt, runStart)   
import LGPT.Numbers (parseLonghand, printLonghand)


--------------------------------------------------------------------------------
{- | Our program. It runs a loop which:
      1. Reads a line of input
      2. Parses it into a structured Request
      3. Does something based on that request (normally printing something out).
-}
runREPL :: IO ()
runREPL = forever $ do
  putStr prompt
  req <- getLine
  respondTo (readRequest req)


--------------------------------------------------------------------------------
-- Parsing and responding to requests:


-- | Our request type, the result of parsing a string.
data Request
  = Unknown
  | Hello 
  deriving (Eq, Ord, Show)


{- | Read a request. 

    This runs the parse function from Megaparsec, and
    converts any failed parses into an Unknown request.
-}
readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left  err -> Unknown
  Right req -> req
  

-- | Currently, the only thing λGPT understands is "Hello"...
parseRequest :: Parser Request
parseRequest = parseHello


parseHello :: Parser Request
parseHello = do
  string "Hello."
  eof
  return Hello


-- | Respond to a request. This is where the behaviours of λGPT will go, but 
-- for now it just responds to "Hello".
respondTo :: Request -> IO ()
respondTo Unknown = putStrLn "I don't understand that."
respondTo Hello = putStrLn "Hi there!"
