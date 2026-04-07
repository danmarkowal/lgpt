{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LGPT.Todo where

import Control.Applicative (optional)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (MonadState (get), StateT)
import Data.ByteString.Char8 qualified as BS
import Data.Csv (Field, FromField (parseField), FromRecord, ToField (toField), ToRecord, encode)
import Data.Csv qualified as Data.Csv.Conversion
import Data.Csv qualified as Data.Csv.Types
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, parseTimeM)
import GHC.Generics (Generic)
import LGPT.Helpers (Parser, dayToday, enumerate, parseDay)
import Text.Megaparsec (many, sepBy1, some, (<|>))
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Char.Lexer (decimal)

todosFile :: String
todosFile = "todos.csv"

data Todo = Todo
  { thing :: !String,
    dueDay :: !Day
  }
  deriving (Generic, Eq, Ord, Show) -- Generic is needed for automatic generation

-- I needed to create a basic encoder/decoder for Day
-- This is so that cassava could derive the remainder of the Todo encoder/decoder
instance ToField Day where
  toField :: Day -> Data.Csv.Types.Field
  toField day =
    BS.pack $ formatTime defaultTimeLocale "%F" day

instance FromField Day where
  parseField :: Field -> Data.Csv.Conversion.Parser Day
  parseField bs =
    case parseTimeM True defaultTimeLocale "%F" (BS.unpack bs) of
      Nothing -> fail "Invalid date (expected YYYY-MM-DD)"
      Just day -> return day

-- Automatically generate instances for paring CSV records
instance FromRecord Todo

instance ToRecord Todo

mkTodo :: String -> TodoDay -> IO Todo
mkTodo thing by =
  do
    ioDay <- fromTodoDay by
    return $
      Todo
        { thing = thing,
          dueDay = ioDay
        }

data TodoDay
  = SpecificDay Day
  | Today
  | Tomorrow
  deriving (Eq, Ord, Show)

-- Converts a todo day to an IO Day
fromTodoDay :: TodoDay -> IO Day
fromTodoDay todoDay = do
  today <- dayToday
  return $ case todoDay of
    SpecificDay day -> day
    Today -> today
    Tomorrow -> addDays 1 today

parseTodoDay :: Parser (Maybe TodoDay)
parseTodoDay = fmap (fmap SpecificDay) parseDay <|> Just Today <$ string "today" <|> Just Tomorrow <$ string "tomorrow"

-- e.g. 1, 2, 3 ...
parseIds :: Parser [Int]
parseIds = decimal `sepBy1` string ", "

-- Represents a todo item with an id
newtype TodoItem = TodoItem {item :: (Int, Todo)}
  deriving (Eq, Ord)

instance Show TodoItem where
  show :: TodoItem -> String
  show (TodoItem (idx, todo)) = show (idx + 1) ++ ". " ++ thing todo

-- Represents a list of todo items
newtype TodoList = TodoList {items :: [TodoItem]}
  deriving (Eq)

instance Show TodoList where
  show :: TodoList -> String
  show (TodoList items) = intercalate "\n" $ map show items

-- Todos are sorted in ascending order of due date
fromTodos :: [Todo] -> TodoList
fromTodos = TodoList . map TodoItem . enumerate . sortBy (comparing dueDay)

toTodos :: TodoList -> [Todo]
toTodos = map (snd . item) . items

filterBefore :: Day -> TodoList -> TodoList
filterBefore day (TodoList items) = TodoList $ filter ((<= day) . dueDay . snd . item) items

removeIds :: [Int] -> TodoList -> TodoList
removeIds ids (TodoList items) = TodoList $ removeFrom items
  where
    idxs :: [Int]
    idxs = map (subtract 1) ids

    removeFrom :: [TodoItem] -> [TodoItem]
    removeFrom = filter ((\idx -> Set.notMember idx (Set.fromList idxs)) . fst . item)
