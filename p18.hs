{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Utils
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Expr
import Control.Applicative

data Expr =
    Lit  Integer
  | Mult Expr Expr
  | Add  Expr Expr deriving (Show)

eval = \case
  Lit  a -> a
  Mult a b -> (eval a) * (eval b)
  Add  a b -> (eval a) + (eval b)

lexeme p = p <* (P.takeWhile (== ' '))
symbol = lexeme . P.char

lparen = symbol '('
rparen = symbol ')'
mult   = symbol '*'
add    = symbol '+'

expr    = buildExpressionParser table term
term    = parens expr <|> (lexeme P.decimal >>= return . Lit)

parens p = do
  lparen
  res <- p
  rparen
  return res

table   = [ [binary add Add AssocLeft ]
          , [binary mult Mult AssocLeft]
          ]

binary name fun assoc = Infix (name >> return fun) assoc

input  = parseInputT "input/input18.txt" (P.many' (expr <* P.endOfLine))
