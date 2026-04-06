-- Created a new module to remove cyclic dependency
{- HLINT ignore "Use newtype instead of data" -}
module LGPT.State where

import Control.Monad (when)
import Control.Monad.Except (Except, ExceptT, MonadError (throwError), runExcept, runExceptT)
import Control.Monad.State (MonadIO (liftIO), MonadState (get), StateT, evalStateT, modify)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Csv (HasHeader (NoHeader))
import Data.Csv qualified as Csv
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Time (Day)
import Data.Vector qualified as V
import LGPT.Expr (EvalError, EvalExpr, EvalState (EvalState, ethat, evars), Expr, Val, evalExpr, handleEvalError)
import LGPT.Helpers (enumerate)
import LGPT.Todo (Todo (Todo), TodoList (items), fromTodos, removeIds, toTodos, todosFile)
import System.Directory (doesFileExist)

-- I chose to use a data structure rather than a tuple since it automatically generates accessors
data LGPTState = LGPTState
  { memory :: Map String String, -- Key-value map of memory (e.g. the sky: is blue)
    vars :: Map String Val,
    that :: Maybe Val, -- Value of the last evaluated expression
    active :: Bool, -- False if the bot should quit
    todos :: [Todo]
  }
  deriving (Show, Eq)

-- Smart constructor used to construct an empty state value
emptyState :: LGPTState
emptyState =
  LGPTState
    { memory = Map.empty,
      vars = Map.empty,
      that = Nothing,
      active = True,
      todos = []
    }

-- Smart constructor which creates EvalState from LGPTState
fromLGPTState :: LGPTState -> EvalState
fromLGPTState LGPTState {vars, that} =
  EvalState
    { evars = vars,
      ethat = that
    }

data LGPTError
  = EvalErr EvalError
  | FutureDayError Day
  | InvalidDayError
  | InvalidInputError
  deriving (Show, Eq)

handleLGPTError :: LGPTError -> IO ()
handleLGPTError err = case err of
  EvalErr err -> handleEvalError err
  FutureDayError day -> putStrLn $ show day ++ " is in the future."
  InvalidDayError -> putStrLn "That's not a real day."
  InvalidInputError -> putStrLn "Your input doesn't look right."

type LGPT = StateT LGPTState (ExceptT LGPTError IO)

-- Makes the chatbot remember a 'thing' by associating it with 'name'
memorise :: String -> String -> LGPTState -> LGPTState
memorise name thing st@LGPTState {memory} = st {memory = Map.insert name thing memory}

-- Recalls a piece of information with the key 'name'
recall :: String -> LGPTState -> Maybe String
recall name LGPTState {memory} = Map.lookup name memory

-- Stores the result of the last evaluated expression
storeThat :: Val -> LGPTState -> LGPTState
storeThat value st@LGPTState {that} = st {that = Just value}

storeVar :: String -> Val -> LGPTState -> LGPTState
storeVar name val st@LGPTState {vars} = st {vars = Map.insert name val vars}

-- Sets bot's state to inactive to indicate that a user has quit
quit :: LGPTState -> LGPTState
quit st = st {active = False}

addTodo :: Todo -> LGPTState -> LGPTState
addTodo todo st@LGPTState {todos} = st {todos = todo : todos}

markDone :: [Int] -> LGPTState -> LGPTState
markDone ids st@LGPTState {todos} = st {todos = toTodos $ removeIds ids $ fromTodos todos}

loadTodos :: LGPT ()
loadTodos = do
  exists <- liftIO $ doesFileExist todosFile
  when exists $ do
    encoded <- liftIO $ BL.readFile todosFile
    case Csv.decode NoHeader encoded of
      Left err -> liftIO $ putStrLn err
      Right decoded -> modify $ \st -> st {todos = V.toList decoded}

saveTodos :: LGPT ()
saveTodos = do
  st <- get
  let encoded = Csv.encode $ todos st
  liftIO $ BL.writeFile todosFile encoded

-- Evaluates an expression and propagates up any evaluation errors
runEval :: EvalExpr a -> LGPT a
runEval f = do
  lgptSt <- get
  let evalSt = fromLGPTState lgptSt
  case runExcept $ evalStateT f evalSt of
    Left err -> throwError (EvalErr err)
    Right res -> return res

-- 'Validates' a Maybe
-- Throws err if it is Nothing, otherwise returns the value in the Maybe
validate :: LGPTError -> Maybe a -> LGPT a
validate err mval = case mval of
  Nothing -> throwError err
  Just val -> return val
