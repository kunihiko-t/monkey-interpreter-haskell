module Parser.Ast (parse) where

import Lexer

data Identifer = Identifer { token :: Token, value :: String }
data Node = Node { token :: Token} deriving (Show)
data Expression = Expression { expression :: Node} deriving (Show)

class Statement a where
    tokenLiteral a = a

data LetStatement = LetStatement { token :: Token, name :: Identifer, value :: Expression} deriving (Statement)


data Program = Program { statements :: []Statement} deriving (Show)

parseStatement :: Token -> Statement

parseStatement (Token { t = LET, literal =  l }) = LetStatement