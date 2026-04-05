{- HLINT ignore "Use newtype instead of data" -}
module LGPT.Expr where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Control.Monad.Except (Except, ExceptT, MonadError (throwError))
import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT, gets)
import LGPT.Helpers (Parser, lexeme, symbol)
import LGPT.Numbers (parseLonghand)
import Text.Megaparsec ((<|>))

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

data EvalError
  = NotEvaluated
  deriving (Show, Eq)

handleEvalError :: EvalError -> IO ()
handleEvalError err = case err of
  NotEvaluated -> putStrLn "I haven't evaluated anything yet."

type EvalExpr = StateT EvalState (Except EvalError) Int

-- For general binary operations between integers
evalBinOp :: (Int -> Int -> Int) -> Expr -> Expr -> EvalExpr
evalBinOp f a b = f <$> evalExpr a <*> evalExpr b

-- Evaluation is a stateful computation which can fail with an error value
evalExpr :: Expr -> EvalExpr
evalExpr (Plus a b) = evalBinOp (+) a b
evalExpr (Minus a b) = evalBinOp (-) a b
evalExpr (Times a b) = evalBinOp (*) a b
evalExpr (Val a) = return a
evalExpr That = do
  st <- get
  case ethat st of
    Nothing -> throwError NotEvaluated
    Just that -> return that
