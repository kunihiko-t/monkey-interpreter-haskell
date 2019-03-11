module Main where
import Control.Lens

import Lexer

infi = do
    a <- getLine
    let b = get_token a
    print b
    infi
  
main = do
    infi
