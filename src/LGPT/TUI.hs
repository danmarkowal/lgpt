{- HLINT ignore "Use <$>" -}
module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The
code that is included here is a good starting point, but you don't need to
keep it if you don't want to.
-}
import Control.Monad (void, when)
import Control.Monad.Except (MonadError (catchError, throwError), runExcept, runExceptT)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (runStateT), evalState, evalStateT, gets, modify)
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
import Data.Time (Day, addDays, dayOfWeek, diffDays)
import LGPT.Expr (Expr, Val (BoolVal, IntVal, StrVal), ValType, asInt, evalExpr, parseArithmeticExpr, parseBoolLiteral, parseExpr, parseIntLiteral, parseVarName)
import LGPT.Helpers (Parser, dayToday, parseDay, parseIdentifier, parseText, prompt, runStart, symbol)
import LGPT.Numbers (parseLonghand, printLonghand)
import LGPT.State (LGPT, LGPTError (FutureDayError, InvalidDayError, InvalidInputError), LGPTState (LGPTState, active, todos), addTodo, emptyState, handleLGPTError, loadTodos, markDone, memorise, quit, recall, runEval, saveTodos, storeThat, storeVar, validate)
import LGPT.Todo (Todo (dueDay, thing), TodoDay, TodoList (items), filterBefore, fromTodoDay, fromTodos, mkTodo, parseIds, parseTodoDay)
import System.Random (randomRIO)
import Text.Megaparsec
  ( MonadParsec (eof),
    choice,
    errorBundlePretty,
    optional,
    parse,
    sepBy1,
    (<|>),
  )
import Text.Megaparsec.Byte.Lexer (decimal)
import Text.Megaparsec.Char (char, space, string)

--------------------------------------------------------------------------------

-- | Our program. It runs a loop which:
--      1. Reads a line of input
--      2. Parses it into a structured Request
--      3. Does something based on that request (normally printing something out).
-- We have to run the state inside of runREPL since the tests expect runREPL to be of type IO ()
-- Before running the REPL we load the list of todo items
runREPL :: IO ()
runREPL = void $ runExceptT $ evalStateT (loadTodos >> loop) emptyState
  where
    loop :: LGPT ()
    loop = do
      liftIO $ putStr prompt
      req <- liftIO getLine
      catchError
        (respondTo $ readRequest req)
        (liftIO . handleLGPTError)
      st <- get
      when (active st) loop

--------------------------------------------------------------------------------
-- Parsing and responding to requests:

data InputType
  = StrInput
  | IntInput
  | BoolInput
  deriving (Eq, Ord)

instance Show InputType where
  show :: InputType -> String
  show inputType = case inputType of
    StrInput -> "some text"
    IntInput -> "a number"
    BoolInput -> "a truth value"

parseInputType :: Parser InputType
parseInputType =
  StrInput
    <$ (symbol "some text" <|> symbol "a string")
      <|> IntInput
    <$ (symbol "a number" <|> symbol "an integer")
      <|> BoolInput
    <$ (symbol "a truth value" <|> symbol "a boolean" <|> symbol "a bool")

newtype InputVal
  = InputVal Val
  deriving (Eq)

-- Strip quotes from strings when printing in the terminal
instance Show InputVal where
  show :: InputVal -> String
  show (InputVal val) = case val of
    StrVal v -> v
    v -> show v

parseInput :: InputType -> String -> LGPT Val
parseInput inputType input = case parse helper "<stdin>" input of
  Left err -> throwError InvalidInputError
  Right val -> return val
  where
    helper :: Parser Val
    helper = case inputType of
      -- We can't use parseStrLiteral because it expects double quotes
      StrInput -> StrVal <$> parseText eof
      IntInput -> parseIntLiteral
      BoolInput -> parseBoolLiteral

-- | Our request type, the result of parsing a string.
data Request
  = Unknown String
  | Hello
  | DayToday
  | DayTomorrow
  | DaysSince (Maybe Day)
  | WhatIs Expr
  | Remember String String
  | TellMeAbout String
  | AddTodo String (Maybe TodoDay)
  | Todos (Maybe TodoDay)
  | Done [Int]
  | Random Expr Expr
  | Let String Expr
  | AskFor InputType
  | Say [Expr]
  | Goodbye
  deriving (Eq, Ord, Show)

-- Represents a block of 'code' (a series of statements one line after another)
type Block = [Request]

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
      parseTellMeAbout,
      parseAddTodo,
      parseTodos,
      parseDone,
      parseRandom,
      parseLet,
      parseAskFor,
      parseSay,
      parseGoodbye
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
  eof
  return $ Remember name thing

parseTellMeAbout :: Parser Request
parseTellMeAbout = do
  string "Tell me about"
  space
  name <- parseIdentifier "."
  char '.'
  return $ TellMeAbout name

parseAddTodo :: Parser Request
parseAddTodo = do
  string "I"
  space
  string "need to" <|> string "have to" <|> string "must"
  space
  string "do"
  space
  -- TODO: Change this parser to allow numerals
  thing <- parseIdentifier "by"
  string "by"
  space
  by <- parseTodoDay
  string "."
  eof
  return $ AddTodo thing by

-- What do I need to (have|need) to do by [date]?
parseTodos :: Parser Request
parseTodos = do
  string "What do I"
  space
  string "have" <|> string "need"
  space
  string "to do by"
  space
  by <- parseTodoDay
  string "?"
  eof
  return $ Todos by

-- I've done [ids].
-- ids -> [decimal] (, [decimal])*
parseDone :: Parser Request
parseDone = do
  string "I've done"
  space
  ids <- parseIds
  string "."
  eof
  return $ Done ids

-- (Give|Generate) (me)? a (random)? number between [min] and [max].
parseRandom :: Parser Request
parseRandom = do
  string "Give" <|> string "Generate"
  space
  optional (string "me" *> space)
  string "a"
  space
  optional (string "random" *> space)
  string "number between"
  space
  min <- parseArithmeticExpr
  space
  string "and"
  space
  max <- parseArithmeticExpr
  string "."
  eof
  return $ Random min max

parseLet :: Parser Request
parseLet = do
  string "Let"
  space
  name <- parseVarName
  space
  string "be"
  space
  expr <- parseExpr
  return $ Let name expr

parseAskFor :: Parser Request
parseAskFor = do
  string "Ask for"
  space
  inputType <- parseInputType
  space
  string "."
  eof
  return $ AskFor inputType

parseSay :: Parser Request
parseSay = do
  string "Say"
  space
  exprs <- parseExpr `sepBy1` string ", "
  string "."
  return $ Say exprs

parseGoodbye :: Parser Request
parseGoodbye = do
  string "Goodbye" <|> string "Bye"
  eof
  return Goodbye

-- | Respond to a request. This is where the behaviours of λGPT will go, but
-- for now it just responds to "Hello".
respondTo :: Request -> LGPT ()
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
respondTo (DaysSince mstart) = do
  today <- liftIO dayToday
  start <- validate InvalidDayError mstart
  let delta = diffDays today start
  when (delta < 0) (throwError $ FutureDayError start)
  liftIO $ putStrLn $ show start ++ " was " ++ show delta ++ " days ago."
respondTo (WhatIs expr) = do
  res <- runEval (evalExpr expr)
  modify $ storeThat res
  liftIO $ putStrLn $ "The answer is " ++ show res ++ "."
respondTo (Remember name thing) = do
  modify $ memorise name thing
  liftIO $ putStrLn "Okay."
respondTo (TellMeAbout name) = do
  mthing <- gets $ recall name
  liftIO $ case mthing of
    Nothing -> putStrLn $ "Sorry, I don't know anything about " ++ name ++ "."
    Just thing -> putStrLn $ "Sure - " ++ name ++ " is " ++ thing ++ "."
respondTo (AddTodo thing mby) = do
  by <- validate InvalidDayError mby
  todo <- liftIO $ mkTodo thing by
  modify $ addTodo todo
  saveTodos
  liftIO $ putStrLn "Noted."
respondTo (Todos mby) = do
  by <- validate InvalidDayError mby >>= liftIO . fromTodoDay
  st <- get
  let todoList = filterBefore by $ fromTodos (todos st)
  liftIO $
    if null (items todoList)
      then putStrLn "Nothing."
      else print todoList
respondTo (Done ids) = do
  modify $ markDone ids
  saveTodos
  liftIO $ putStrLn "Noted."
respondTo (Random minExpr maxExpr) = do
  min <- runEval (evalExpr minExpr >>= asInt)
  max <- runEval (evalExpr maxExpr >>= asInt)
  num <- liftIO $ randomRIO (min, max)
  modify $ storeThat (IntVal num)
  liftIO $ putStrLn $ "Your number is " ++ printLonghand num ++ "."
respondTo (Let name expr) = do
  val <- runEval (evalExpr expr)
  modify $ storeVar name val
  liftIO $ putStrLn "Stored."
respondTo (AskFor inputType) = do
  liftIO $ putStr $ "Enter " ++ show inputType ++ ": "
  input <- liftIO getLine
  val <- parseInput inputType input
  modify $ storeThat val
  liftIO $ putStrLn "Thank you!"
respondTo (Say exprs) = do
  vals <- mapM (runEval . evalExpr) exprs
  liftIO $ putStrLn $ unwords $ map (show . InputVal) vals
respondTo Goodbye = do
  liftIO $ putStrLn "See you next time!"
  modify quit
