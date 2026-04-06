{- HLINT ignore "Use newtype instead of data" -}
module LGPT.Expr where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Control.Monad.Except (Except, ExceptT, MonadError (throwError))
import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT, gets)
import Data.Map (Map)
import LGPT.Helpers (Parser, lexeme, symbol)
import LGPT.Numbers (parseLonghand, printLonghand)
import Text.Megaparsec ((<|>))

data ValType
  = IntType
  | BoolType
  deriving (Eq, Ord)

instance Show ValType where
  show :: ValType -> String
  show valType = case valType of
    IntType -> "a number"
    BoolType -> "a truth value"

-- Our "programming language" is dynamically typed
-- This means that the types of values are checked when expressions are evaluated
-- If the value of an expression is wrong (e.g. true plus two) then an evaluation error is raised
data Val
  = IntVal Int -- e.g. "one plus two" -> IntVal 3
  | BoolVal Bool -- e.g. "true and true" -> BoolVal True
  deriving (Eq, Ord)

instance Show Val where
  show :: Val -> String
  show val = case val of
    IntVal v -> printLonghand v
    BoolVal v -> if v then "true" else "false"

typeOf :: Val -> ValType
typeOf val = case val of
  IntVal _ -> IntType
  BoolVal _ -> BoolType

-- https://stackoverflow.com/questions/6047522/haskell-type-and-pattern-matching-question-extracting-fields-from-a-data-type
asInt :: Val -> EvalExpr Int
asInt val = case val of
  IntVal v -> return v
  v -> throwError $ UnexpectedType IntType (typeOf v)

asBool :: Val -> EvalExpr Bool
asBool val = case val of
  BoolVal v -> return v
  v -> throwError $ UnexpectedType IntType (typeOf v)

-- Tree-like data structure for expressions
data Expr
  = Plus Expr Expr -- IntVal operations
  | Minus Expr Expr
  | Times Expr Expr
  | -- | Over Expr Expr
    And Expr Expr -- Boolean operations
  | Or Expr Expr
  | Is Expr Expr -- "=="
  | IsNot Expr Expr -- "/="
  | IsGreaterThan Expr Expr -- ">"
  | IsLessThan Expr Expr -- "<"
  | IsAtLeast Expr Expr -- ">="
  | IsAtMost Expr Expr -- "<="
  | Literal Val -- Literals and variables
  | Var String -- e.g. "x"
  | That -- "that"
  deriving (Eq, Ord, Show)

parseLiteral :: Parser Expr
parseLiteral = parseIntLiteral <|> parseBoolLiteral
  where
    parseIntLiteral :: Parser Expr
    parseIntLiteral = Literal . IntVal <$> lexeme parseLonghand

    parseBoolLiteral :: Parser Expr
    parseBoolLiteral = Literal (BoolVal True) <$ symbol "true" <|> Literal (BoolVal False) <$ symbol "false"

parseThat :: Parser Expr
parseThat = That <$ symbol "that"

parseTerm :: Parser Expr
parseTerm = parseLiteral <|> parseThat

-- I decided to use makeExprParser from the parser-combinators Hackage
-- I could have used a simpler parser (since all operators are left-associative and have the same precedence)
-- where we keep track of the 'current value' and parse the operator and next literal value and update the current value accordingly
-- However, using an operator table is more flexible (we can change precedence, add more operators) and declarative
parseExpr :: Parser Expr
parseExpr = makeExprParser parseTerm opTable

-- Arithmetic operations: highest precedence
-- Comparisons: medium precedence
-- Boolean operators: lowest precedence
-- e.g. "two plus two is at least four and ten is nine plus one" is interpreted as
--      ((2 + 2) >= (4)) && ((10) == (9 + 1))
opTable :: [[Operator Parser Expr]]
opTable =
  [ [ InfixL (Plus <$ symbol "plus"),
      InfixL (Minus <$ symbol "minus"),
      InfixL (Times <$ symbol "times")
    ],
    [ InfixL (IsGreaterThan <$ symbol "is greater than"),
      InfixL (IsLessThan <$ symbol "is less than"),
      InfixL (IsAtLeast <$ symbol "is at least"),
      InfixL (IsAtMost <$ symbol "is at most"),
      InfixL (Is <$ symbol "is"),
      InfixL (IsNot <$ symbol "is not")
    ],
    [ InfixL (And <$ symbol "and"),
      InfixL (Or <$ symbol "or")
    ]
  ]

-- This is its own separate data type as I might add more features to the evaluator later
data EvalState
  = EvalState
  { evars :: Map String Val,
    ethat :: Maybe Val
  }
  deriving (Show, Eq)

data EvalError
  = NotEvaluated
  | UnexpectedType ValType ValType
  deriving (Show, Eq)

handleEvalError :: EvalError -> IO ()
handleEvalError err = case err of
  NotEvaluated -> putStrLn "I haven't evaluated anything yet."
  UnexpectedType expected got -> putStrLn $ "I expected " ++ show expected ++ " but I got " ++ show got ++ "."

type EvalExpr = StateT EvalState (Except EvalError)

-- For general binary operations between integers
evalBinOp :: (a -> a -> b) -> (Val -> EvalExpr a) -> Expr -> Expr -> EvalExpr b
evalBinOp f cast a b = f <$> (evalExpr a >>= cast) <*> (evalExpr b >>= cast)

-- Evaluation is a stateful computation which can fail with an error value
evalExpr :: Expr -> EvalExpr Val
evalExpr (Plus a b) = IntVal <$> evalBinOp (+) asInt a b
evalExpr (Minus a b) = IntVal <$> evalBinOp (-) asInt a b
evalExpr (Times a b) = IntVal <$> evalBinOp (*) asInt a b
evalExpr (And a b) = BoolVal <$> evalBinOp (&&) asBool a b
evalExpr (Or a b) = BoolVal <$> evalBinOp (||) asBool a b
evalExpr (Is a b) = BoolVal <$> evalBinOp (==) return a b -- Checks type and value
evalExpr (IsNot a b) = BoolVal <$> evalBinOp (/=) return a b
evalExpr (IsGreaterThan a b) = BoolVal <$> evalBinOp (>) asInt a b
evalExpr (IsLessThan a b) = BoolVal <$> evalBinOp (<) asInt a b
evalExpr (IsAtLeast a b) = BoolVal <$> evalBinOp (>=) asInt a b
evalExpr (IsAtMost a b) = BoolVal <$> evalBinOp (<=) asInt a b
evalExpr (Literal a) = return a
evalExpr That = do
  st <- get
  case ethat st of
    Nothing -> throwError NotEvaluated
    Just that -> return that
