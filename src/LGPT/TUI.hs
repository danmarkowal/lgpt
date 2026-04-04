{- HLINT ignore "Use <$>" -}
module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The
code that is included here is a good starting point, but you don't need to
keep it if you don't want to.
-}
import Control.Monad
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (runStateT), evalStateT, gets, modify)
import Data.Time (Day, addDays, dayOfWeek, diffDays)
import LGPT.Helpers (LGPTState (that), Parser, dayToday, emptyState, memorise, parseDay, parseIdentifier, prompt, recall, runStart, storeThat)
import LGPT.Numbers (EvalState (EvalState, ethat), Expr, evalExpr, fromLGPTState, parseExpr, parseLonghand, printLonghand)
import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------

-- | Our program. It runs a loop which:
--      1. Reads a line of input
--      2. Parses it into a structured Request
--      3. Does something based on that request (normally printing something out).
runREPL :: IO ()
runREPL =
  evalStateT
    ( forever $ do
        liftIO $ putStr prompt
        req <- liftIO getLine
        respondTo (readRequest req)
    )
    emptyState

--------------------------------------------------------------------------------
-- Parsing and responding to requests:

-- | Our request type, the result of parsing a string.
data Request
  = Unknown String
  | Hello
  | DayToday
  | DayTomorrow
  | DaysSince Day
  | WhatIs Expr
  | Remember String String
  | TellMeAbout String
  deriving (Eq, Ord, Show)

-- | Read a request.
--
--    This runs the parse function from Megaparsec, and
--    converts any failed parses into an Unknown request.
readRequest :: String -> Request
readRequest str = case parse parseRequest "<stdin>" str of
  Left err -> Unknown (errorBundlePretty err)
  Right req -> req

parseRequest :: Parser Request
parseRequest =
  choice
    [ parseHello,
      parseDayToday,
      parseDayTomorrow,
      parseDaysSince,
      parseWhatIs,
      parseRemember,
      parseTellMeAbout
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

parseRemember :: Parser Request
parseRemember = do
  string "Remember that"
  space
  name <- parseIdentifier "is"
  space
  string "is"
  space
  thing <- parseIdentifier "."
  char '.'
  return $ Remember name thing

parseTellMeAbout :: Parser Request
parseTellMeAbout = do
  string "Tell me about"
  space
  name <- parseIdentifier "."
  char '.'
  return $ TellMeAbout name

-- | Respond to a request. This is where the behaviours of λGPT will go, but
-- for now it just responds to "Hello".
respondTo :: Request -> StateT LGPTState IO ()
respondTo (Unknown err) = liftIO $ do
  putStrLn "I don't understand that."
  putStrLn err
respondTo Hello = liftIO $ putStrLn "Hi there!"
respondTo DayToday = liftIO $ do
  dotw <- dayOfWeek <$> dayToday
  putStrLn $ "Today is " ++ show dotw ++ "."
respondTo DayTomorrow = liftIO $ do
  today <- dayToday
  let dotw = dayOfWeek $ addDays 1 today
  putStrLn $ "Tomorrow is " ++ show dotw ++ "."
respondTo (DaysSince start) = liftIO $ do
  today <- dayToday
  -- TODO: Prevent this from returning negatives
  let delta = diffDays today start
  putStrLn $ show start ++ " was " ++ show delta ++ " days ago."
respondTo (WhatIs expr) = do
  lgptSt <- get
  let evalSt = fromLGPTState lgptSt
  -- Have to run evalExpr separately since it returns a different kind of computation
  case evalStateT (evalExpr expr) evalSt of
    Left err -> liftIO $ putStrLn err
    Right res -> do
      liftIO $ putStrLn $ "The answer is " ++ printLonghand res ++ "."
      modify $ storeThat res
respondTo (Remember name thing) = do
  modify $ memorise name thing
  liftIO $ putStrLn "Okay."
respondTo (TellMeAbout name) = do
  mthing <- gets $ recall name
  liftIO $ case mthing of
    Nothing -> putStrLn $ "Sorry, I don't know anything about " ++ name ++ "."
    Just thing -> putStrLn $ "Sure - " ++ name ++ " is " ++ thing ++ "."
