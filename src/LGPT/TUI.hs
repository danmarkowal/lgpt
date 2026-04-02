{- HLINT ignore "Use <$>" -}
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
import LGPT.Helpers (Parser, prompt, runStart, parseDay, dayToday)
import LGPT.Numbers (parseLonghand, printLonghand, Expr, parseExpr, evalExpr)
import Data.Time (dayOfWeek, Day, addDays, diffDays)


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
  | DayToday
  | DayTomorrow
  | DaysSince Day
  | WhatIs Expr
  deriving (Eq, Ord, Show)


{- | Read a request. 

    This runs the parse function from Megaparsec, and
    converts any failed parses into an Unknown request.
-}
readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left  err -> Unknown
  Right req -> req


parseRequest :: Parser Request
parseRequest
  = choice
  [ parseHello
  , parseDayToday
  , parseDayTomorrow
  , parseDaysSince
  , parseWhatIs
  ]


parseHello :: Parser Request
parseHello = do
  string "Hello"
  eof
  return Hello


parseDayToday :: Parser Request
parseDayToday = do
  string "What day is it?" <|> string "What day is it today?"
  eof
  return DayToday


parseDayTomorrow :: Parser Request
parseDayTomorrow = do
  string "What day is it tomorrow?"
  eof
  return DayTomorrow


parseDaysSince :: Parser Request
parseDaysSince = do
  string "How long ago was"
  space
  day <- parseDay
  char '?'
  eof
  return (DaysSince day)


parseWhatIs :: Parser Request
parseWhatIs = do
  string "What is"
  space
  expr <- parseExpr
  char '?'
  eof
  return $ WhatIs expr


-- | Respond to a request. This is where the behaviours of λGPT will go, but 
-- for now it just responds to "Hello".
respondTo :: Request -> IO ()
respondTo Unknown = putStrLn "I don't understand that."
respondTo Hello = putStrLn "Hi there!"
respondTo DayToday = do
  dotw <- dayOfWeek <$> dayToday
  putStrLn $ "Today is " ++ show dotw ++ "."
respondTo DayTomorrow = do
  today <- dayToday
  let dotw = dayOfWeek $ addDays 1 today
  putStrLn $ "Tomorrow is " ++ show dotw ++ "."
respondTo (DaysSince start) = do
  today <- dayToday
  -- TODO: Prevent this from returning negatives
  let delta = diffDays today start
  putStrLn $ show start ++ " was " ++ show delta ++ " days ago."
respondTo (WhatIs expr) = do
  let res = evalExpr expr
  putStrLn $ "The answer is " ++ printLonghand res ++ "."
