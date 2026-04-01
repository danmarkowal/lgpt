{- HLINT ignore "Use <$>" -}
module LGPT.Helpers where

import Text.Megaparsec
import Data.Void (Void)
import System.Console.ANSI
import System.IO
import Control.Monad (when)
import Data.Time (Day, dayOfWeek, LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getZonedTime, fromGregorian, parseTimeM, defaultTimeLocale)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

-- | Our Parser monad is a specialisation of the complex "Parsec" type 
-- so that it works on strings. 
type Parser = Parsec Void String


-- | The prompt to show the user when asking for input.
-- Feel free to change this to something else.
prompt :: String
prompt = "λ> "

digits :: String
digits = ['0'..'9']

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
    mapM_ putStrLn
      [ "    ___     _____ _____ _______  "
      , "    \\  \\   / ____|  __ \\__   __| "
      , "     \\  \\ | |  __| |__) | | |    "
      , "      >  \\| | |_ |  ___/  | |    "
      , "     / /\\ | |__| | |      | |    "
      , "    /_/  \\_\\_____|_|      |_|    "
      , "The Lightly Generalised Parsing Task"
      , ""
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
