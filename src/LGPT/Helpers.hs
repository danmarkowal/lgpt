{- HLINT ignore "Use <$>" -}
module LGPT.Helpers where

import Control.Applicative (Alternative)
import Control.Monad (when)
import Data.Foldable qualified as Map
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Time (Day, LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), dayOfWeek, defaultTimeLocale, fromGregorian, getZonedTime, parseTimeM)
import Data.Void (Void)
import System.Console.ANSI
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char (char, letterChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- | Our Parser monad is a specialisation of the complex "Parsec" type
-- so that it works on strings.
type Parser = Parsec Void String

-- | The prompt to show the user when asking for input.
-- Feel free to change this to something else.
prompt :: String
prompt = "λ> "

-- | Set up the interactive terminal session.
runStart :: IO ()
runStart = do
  isTTY <- hIsTerminalDevice stdin

  -- Only do the interactive setup when running interactively.
  when isTTY $ do
    -- Clear away the stack build details.
    clearScreen

    -- We set this so the prompt will always print out right away.
    hSetBuffering stdout NoBuffering

    -- Change text to magenta
    setSGR [SetColor Foreground Vivid Magenta]

    -- Print a nice picture
    mapM_
      putStrLn
      [ "    ___     _____ _____ _______  ",
        "    \\  \\   / ____|  __ \\__   __| ",
        "     \\  \\ | |  __| |__) | | |    ",
        "      >  \\| | |_ |  ___/  | |    ",
        "     / /\\ | |__| | |      | |    ",
        "    /_/  \\_\\_____|_|      |_|    ",
        "The Lightly Generalised Parsing Task",
        ""
      ]

    -- Unset the colour
    setSGR [Reset]

-- And now we are ready!

-- Parses a day in the form '%Y-%M-%D'
parseDay :: Parser Day
parseDay = do
  year <- decimal
  char '-'
  month <- decimal
  char '-'
  day <- decimal
  -- TODO: Use fromGregorianValid to prevent invalid dates from being accepted
  return $ fromGregorian year month day

-- Returns the day today according to local time
dayToday :: IO Day
dayToday = localDay . zonedTimeToLocalTime <$> getZonedTime

-- Eat exactly one space between parsers
lexeme :: Parser a -> Parser a
lexeme p = p <* space

-- Lexeme with a string parser
symbol :: String -> Parser String
symbol s = lexeme (string s)

-- Parses an identifier which ends with a delimiter (e.g. 'is')
parseIdentifier :: String -> Parser String
parseIdentifier delimiter = unwords <$> manyTill word (lookAhead (string delimiter))
  where
    word = some letterChar <* space

data LGPTState = LGPTState
  { memory :: Map String String, -- Key-value map of memory (e.g. the sky: is blue)
    that :: Maybe Int -- Value of the last evaluated expression
  }

emptyState :: LGPTState
emptyState =
  LGPTState
    { memory = Map.empty,
      that = Nothing
    }

memorise :: String -> String -> LGPTState -> LGPTState
memorise name thing state@LGPTState {memory} = state {memory = Map.insert name thing memory}

recall :: String -> LGPTState -> Maybe String
recall name state@LGPTState {memory} = Map.lookup name memory

storeThat :: Int -> LGPTState -> LGPTState
storeThat value state@LGPTState {that} = state {that = Just value}
