{- HLINT ignore "Use <$>" -}
{- HLINT ignore "Use newtype instead of data" -}
module LGPT.Numbers where

import Control.Applicative
import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.State
import LGPT.Helpers (LGPTState (LGPTState, that), Parser, lexeme, symbol)
import Text.Megaparsec
import Text.Megaparsec.Char

--------------------------------------------------------------------------------

-- | Print an integer in longhand form.
--
--    You don't need to touch this, but feel free to read it and try to work out what it's doing :)
--
--    You can open `stack repl` and try out `printLonghand` on some numbers to see how it works. Examples are also given in test/Spec.hs.
printLonghand :: Int -> String
printLonghand n
  | n > 1000000 = "an unfathomably large number"
  | n == 1000000 = "one million"
  | n == 0 = "zero"
  | n >= 1000 =
      let thouStr = printLonghand (n `div` 1000) ++ " thousand"
          and = if n `mod` 1000 >= 100 then " " else " and "
          hundStr = printLonghand (n `mod` 1000)
       in if n `mod` 1000 == 0
            then thouStr
            else thouStr ++ and ++ hundStr
  | n >= 100 =
      let hundStr = printLonghand (n `div` 100) ++ " hundred"
          tensStr = printLonghand (n `mod` 100)
       in if n `mod` 100 == 0
            then hundStr
            else hundStr ++ " and " ++ tensStr
  | n >= 20 =
      let tensStr = tens !! ((n `div` 10) - 2)
          hyphen = if n `mod` 10 /= 0 then "-" else ""
          unitStr = ("" : oneToNine) !! (n `mod` 10)
       in tensStr ++ hyphen ++ unitStr
  | n > 0 = ("" : oneToNine ++ tenToTwenty) !! n
  | otherwise = "a negative number"

--------------------------------------------------------------------------------

-- | Parse an integer in longhand form.
--
--    You don't need to touch this, but feel free to read it and try to work out what it's doing :)
--
--    This parser is a lot more complicated than the ones you need to write.
parseLonghand :: Parser Int
parseLonghand =
  choice
    [ string "one million" >> pure 1000000,
      try parseSubMillion,
      try parseSubThousand,
      try parseSubHundred,
      try parseUnit,
      string "zero" >> pure 0
    ]
  where
    parseSubMillion :: Parser Int
    parseSubMillion = do
      th <- choice [try parseSubThousand, parseSubHundred]
      string " thousand"
      rest <-
        choice
          [ string " and " >> parseSubHundred,
            string " " >> parseSubThousand,
            pure 0
          ]
      pure $ th * 1000 + rest

    parseSubThousand :: Parser Int
    parseSubThousand = do
      h <- parseUnit
      string " hundred"
      rest <- (string " and " >> parseSubHundred) <|> pure 0
      pure $ h * 100 + rest

    parseSubHundred :: Parser Int
    parseSubHundred =
      choice
        [ do
            tens <- choice $ zipWith (\v s -> string s >> pure v) [20, 30 .. 90] tens
            rest <- (string "-" >> parseUnit) <|> pure 0
            pure $ tens + rest,
          parseTenToTwenty,
          parseUnit
        ]

    parseTenToTwenty :: Parser Int
    parseTenToTwenty = choice $ zipWith (\v s -> string s >> pure v) [10 .. 19] tenToTwenty

    parseUnit :: Parser Int
    parseUnit =
      choice $
        zipWith
          (\v s -> string s >> pure v)
          [1 .. 9]
          oneToNine

--------------------------------------------------------------------------------
-- Helpers used for both parsing and printing

oneToNine :: [String]
oneToNine =
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

tenToTwenty :: [String]
tenToTwenty =
  [ "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen"
  ]

tens :: [String]
tens =
  [ "twenty",
    "thirty",
    "forty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety"
  ]

--------------------------------------------------------------------------------
-- Helpers used to parse and evaluate number expressions

-- Tree-like data structure for expressions
data Expr
  = Plus Expr Expr
  | Minus Expr Expr
  | Times Expr Expr
  | Val Int
  | That
  deriving (Eq, Ord, Show)

parseVal :: Parser Expr
parseVal = Val <$> lexeme parseLonghand

parseThat :: Parser Expr
parseThat = That <$ symbol "that"

parseTerm :: Parser Expr
parseTerm = parseVal <|> parseThat

-- I decided to use makeExprParser from the parser-combinators Hackage
-- I could have used a simpler parser (since all operators are left-associative and have the same precedence)
-- where we keep track of the 'current value' and parse the operator and next literal value and update the current value accordingly
-- However, using an operator table is more flexible (we can change precedence, add more operators) and declarative
parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm opTable

-- Only one precedence level since all operators have the same precedence
opTable :: [[Operator Parser Expr]]
opTable =
  [ [ InfixL (Plus <$ symbol "plus"),
      InfixL (Minus <$ symbol "minus"),
      InfixL (Times <$ symbol "times")
    ]
  ]

-- This is its own separate data type as I might add more features to the evaluator later
data EvalState
  = EvalState {ethat :: Maybe Int}
  deriving (Show, Eq)

-- Smart constructor which creates EvalState from LGPTState
fromLGPTState :: LGPTState -> EvalState
fromLGPTState LGPTState {that} =
  EvalState
    { ethat = that
    }

-- For general binary operations between integers
evalBinOp :: (Int -> Int -> Int) -> Expr -> Expr -> StateT EvalState (Either String) Int
evalBinOp f a b = f <$> evalExpr a <*> evalExpr b

-- Evaluation is a stateful computation which can fail with an error value
evalExpr :: Expr -> StateT EvalState (Either String) Int
evalExpr (Plus a b) = evalBinOp (+) a b
evalExpr (Minus a b) = evalBinOp (-) a b
evalExpr (Times a b) = evalBinOp (*) a b
evalExpr (Val a) = return a
evalExpr That = do
  mthat <- gets ethat
  case mthat of
    Nothing -> lift $ Left "I haven't evaluated anything yet."
    Just that -> return that
