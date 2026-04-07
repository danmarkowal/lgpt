{- HLINT ignore "Use <$>" -}
module LGPT.TUI where

{-
This file is the main entry point to your coursework.

You can create or modify any files in src/ as much as you like. The
code that is included here is a good starting point, but you don't need to
keep it if you don't want to.
-}
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Combinators (empty)
import Control.Monad.Except (MonadError (catchError, throwError), runExcept, runExceptT)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT (runStateT), evalState, evalStateT, execStateT, gets, modify)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Time (Day, addDays, dayOfWeek, diffDays)
import GHC.TopHandler (runIO)
import LGPT.Expr (Expr, Val (BoolVal, IntVal, StrVal), ValType, asBool, asInt, asStr, evalExpr, parseArithmeticExpr, parseBoolLiteral, parseExpr, parseIntLiteral, parseVarName)
import LGPT.Helpers (Parser, dayToday, parseDay, parseIdentifier, parseText, prompt, runStart, symbol)
import LGPT.Numbers (parseLonghand, printLonghand)
import LGPT.State (LGPT, LGPTError (FutureDayError, InvalidDayError, InvalidInputError, MaxDepthReached), LGPTState (LGPTState, active, depth, todos), addTodo, emptyState, ensureFileExists, handleLGPTError, loadTodos, markDone, maxDepth, memorise, mkRunState, quit, recall, returnFromRun, runEval, runUserShell, saveTodos, storeThat, storeVar, validate)
import LGPT.Todo (Todo (dueDay, thing), TodoDay, TodoList (items), filterBefore, fromTodoDay, fromTodos, mkTodo, parseIds, parseTodoDay)
import System.Random (randomRIO)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Pos,
    choice,
    errorBundlePretty,
    many,
    optional,
    parse,
    sepBy1,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char, eol, hspace, hspace1, space, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

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

-- The type of input we can ask a user for
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

-- Used in parseAskFor to determine the input to ask for
parseInputType :: Parser InputType
parseInputType =
  StrInput
    <$ (symbol "some text" <|> symbol "a string")
      <|> IntInput
    <$ (symbol "a number" <|> symbol "an integer")
      <|> BoolInput
    <$ (symbol "a truth value" <|> symbol "a boolean" <|> symbol "a bool")

-- A wrapper around val which we use to print values with "Say"
newtype InputVal
  = InputVal Val
  deriving (Eq)

-- Strip quotes from strings when printing in the terminal
instance Show InputVal where
  show :: InputVal -> String
  show (InputVal val) = case val of
    StrVal v -> v
    v -> show v

-- Parses the input entered by the user to ensure it is of the correct form
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
  | Run Expr
  | IfOtherwise Expr Block (Maybe Block)
  | RepeatUntil Block Expr
  | Goodbye
  deriving (Eq, Ord, Show)

-- Represents a block of 'code' (a series of statements one line after another)
type Block = [Request]

runBlock :: Block -> LGPT ()
runBlock = mapM_ respondTo

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
      try parseAddTodo, -- Needed because parseDone also starts with 'I'
      parseTodos,
      parseDone,
      parseRandom,
      parseLet,
      parseAskFor,
      parseSay,
      parseRun,
      parseIfOtherwise,
      parseRepeatUntil,
      parseGoodbye
    ]

parseHello :: Parser Request
parseHello = do
  string "Hello"
  optional $ string "." -- For some reason the tests fail if a full stop is required
  endRequest
  return Hello

parseDayToday :: Parser Request
parseDayToday = do
  string "What day is it?" <|> string "What day is it today?"
  endRequest
  return DayToday

parseDayTomorrow :: Parser Request
parseDayTomorrow = do
  string "What day is it tomorrow?"
  endRequest
  return DayTomorrow

parseDaysSince :: Parser Request
parseDaysSince = do
  string "How long ago was"
  space
  day <- parseDay
  char '?'
  endRequest
  return (DaysSince day)

parseWhatIs :: Parser Request
parseWhatIs = do
  string "What is"
  space
  expr <- parseExpr
  char '?'
  endRequest
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
  endRequest
  return $ Remember name thing

parseTellMeAbout :: Parser Request
parseTellMeAbout = do
  string "Tell me about"
  space
  name <- parseIdentifier "."
  char '.'
  endRequest
  return $ TellMeAbout name

parseAddTodo :: Parser Request
parseAddTodo = do
  string "I"
  space
  string "need to" <|> string "have to" <|> string "must"
  space
  -- TODO: Change this parser to allow numerals
  thing <- parseIdentifier "by"
  string "by"
  space
  by <- parseTodoDay
  string "."
  endRequest
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
  endRequest
  return $ Todos by

-- I've done [ids].
-- ids -> [decimal] (, [decimal])*
parseDone :: Parser Request
parseDone = do
  string "I've done"
  space
  ids <- parseIds
  string "."
  endRequest
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
  endRequest
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
  string "."
  endRequest
  return $ Let name expr

parseAskFor :: Parser Request
parseAskFor = do
  string "Ask for"
  space
  inputType <- parseInputType
  space
  string "."
  endRequest
  return $ AskFor inputType

-- Say [expr][, expr]*.
parseSay :: Parser Request
parseSay = do
  string "Say"
  space
  exprs <- parseExpr `sepBy1` string ", "
  string "."
  endRequest
  return $ Say exprs

parseRun :: Parser Request
parseRun = do
  string "Run"
  space
  expr <- parseExpr
  string "."
  endRequest
  return $ Run expr

-- The worst thing I've ever written
parseIfOtherwise :: Parser Request
parseIfOtherwise = do
  ref <- L.indentLevel
  parseIf False ref
  where
    parseIf :: Bool -> Pos -> Parser Request
    parseIf chain ref = do
      string (if chain then "if" else "If")
      space
      expr <- parseExpr
      space
      string "then"
      eol
      trueBlock <- parseBlock ref
      falseBlock <- parseOtherwise ref
      return $ IfOtherwise expr trueBlock falseBlock

    parseOtherwise :: Pos -> Parser (Maybe Block)
    parseOtherwise ref = optional . try $ do
      L.indentGuard spaceConsumer EQ ref
      string "Otherwise"
      parseOtherwiseTail ref

    parseOtherwiseTail :: Pos -> Parser Block
    parseOtherwiseTail ref =
      try
        ( do
            hspace -- Consume the space between "Otherwise" and "if"
            res <- parseIf True ref -- Parse another if statement but lowercase
            return [res]
        )
        <|> (eol *> parseBlock ref)

parseRepeatUntil :: Parser Request
parseRepeatUntil = do
  ref <- L.indentLevel
  string "Repeat"
  eol
  block <- parseBlock ref
  condition <- parseUntil ref
  return $ RepeatUntil block condition
  where
    parseUntil :: Pos -> Parser Expr
    parseUntil ref = do
      L.indentGuard spaceConsumer EQ ref
      string "Until"
      space
      condition <- parseExpr
      string "."
      endRequest
      return condition

parseGoodbye :: Parser Request
parseGoodbye = do
  string "Goodbye" <|> string "Bye"
  optional $ string "."
  eof
  return Goodbye

endRequest :: Parser ()
endRequest = void eol <|> eof

-- Parses a block of requests/statements
parseBlock :: Pos -> Parser Block
parseBlock ref = do
  -- Find the indent of the first request ensure it is > ref
  pos <- L.indentGuard spaceConsumer GT ref
  first <- parseRequest

  -- Parse any remaining requests at that indentation level
  rest <- many $ try $ do
    L.indentGuard spaceConsumer EQ pos
    parseRequest

  return (first : rest)

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

parseProgram :: Parser Block
parseProgram = some parseRequest

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
-- WhatIs is actually useful to 'return' values from 'functions'
respondTo (WhatIs expr) = do
  res <- runEval (evalExpr expr)
  modify $ storeThat res
  runUserShell $ liftIO $ putStrLn $ "The answer is " ++ show res ++ "."
-- This is also kind of useless in programs
respondTo (Remember name thing) = runUserShell $ do
  modify $ memorise name thing
  liftIO $ putStrLn "Okay."
-- Todo management should only be accessible by the user
respondTo (TellMeAbout name) = runUserShell $ do
  mthing <- gets $ recall name
  liftIO $ case mthing of
    Nothing -> putStrLn $ "Sorry, I don't know anything about " ++ name ++ "."
    Just thing -> putStrLn $ "Sure - " ++ name ++ " is " ++ thing ++ "."
respondTo (AddTodo thing mby) = runUserShell $ do
  by <- validate InvalidDayError mby
  todo <- liftIO $ mkTodo thing by
  modify $ addTodo todo
  saveTodos
  liftIO $ putStrLn "Noted."
respondTo (Todos mby) = runUserShell $ do
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
  runUserShell $ liftIO $ putStrLn "Noted."
respondTo (Random minExpr maxExpr) = do
  min <- runEval (evalExpr minExpr >>= asInt)
  max <- runEval (evalExpr maxExpr >>= asInt)
  num <- liftIO $ randomRIO (min, max)
  modify $ storeThat (IntVal num)
  runUserShell $ liftIO $ putStrLn $ "Your number is " ++ printLonghand num ++ "."
respondTo (Let name expr) = do
  val <- runEval (evalExpr expr)
  modify $ storeVar name val
  runUserShell $ liftIO $ putStrLn "Stored."
respondTo (AskFor inputType) = do
  liftIO $ putStr $ "Enter " ++ show inputType ++ ": "
  input <- liftIO getLine
  val <- parseInput inputType input
  modify $ storeThat val
  runUserShell $ liftIO $ putStrLn "Thank you!"
respondTo (Say exprs) = do
  vals <- mapM (runEval . evalExpr) exprs
  runUserShell $ liftIO $ putStrLn $ unwords $ map (show . InputVal) vals
respondTo (Run expr) = do
  filename <- runEval (evalExpr expr >>= asStr)
  ensureFileExists filename
  content <- liftIO $ readFile filename
  -- Empty lines are ignored
  let src = intercalate "\n" $ filter (not . null) $ lines content
  program <- case parse parseProgram filename src of
    Left err -> return [Unknown (errorBundlePretty err)]
    Right val -> return val
  st <- get
  when (depth st == maxDepth) (throwError MaxDepthReached)
  res <- liftIO $ runExceptT $ execStateT (runBlock program) (mkRunState st)
  case res of
    Left err -> throwError err
    Right runSt -> modify $ returnFromRun runSt
respondTo (IfOtherwise conditionExpr trueBlock falseBlock) = do
  condition <- runEval (evalExpr conditionExpr >>= asBool)
  runBlock $
    if condition
      then trueBlock
      else fromMaybe [] falseBlock
respondTo (RepeatUntil block conditionExpr) = do
  let loop = do
        runBlock block
        condition <- runEval (evalExpr conditionExpr >>= asBool)
        unless condition loop
  loop
respondTo Goodbye = do
  liftIO $ putStrLn "See you next time!"
  modify quit
