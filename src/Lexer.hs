module Lexer (get_token, Token, TokenType) where

get_letters :: [Char] ->  [Char]
get_letters (x:xs)
    | elem x (['A'..'Z'] ++ ['a' .. 'z']) = x : get_letters xs
    | otherwise = ""
get_letters [] = ""

get_numbers :: [Char] ->  [Char]
get_numbers (x:xs)
    | elem x ['0'..'9'] = x : get_numbers xs
    | otherwise = ""
get_numbers [] = ""



data TokenType = PLUS | MINUS | ASTERISK |  BANG | EQUAL | ASSIGN |  NOT_EQUAL | LPAREN | RPAREN | SEMICOLON | IDENT | NUMBER | LBRACE | RBRACE | COMMA | LET | INT | SLASH | LT | GT | EOF deriving (Show)

data Token = Token { t :: TokenType, literal :: [Char]} deriving (Show)

get_token :: [Char] -> [Token]
get_token (';':xi) = [Token{ t = SEMICOLON, literal = ";"}] ++ get_token xi
get_token ('+':xi) = [Token{ t = PLUS, literal = "+"}] ++ get_token xi
get_token ('*':xi) = [Token{ t = ASTERISK, literal = "*"}] ++ get_token xi
get_token ('-':xi) = [Token{ t = MINUS, literal = "-"}] ++ get_token xi
get_token ('(':xi) = [Token{ t = LPAREN, literal = "("}] ++ get_token xi
get_token (')':xi) = [Token{ t = RPAREN, literal = ")"}] ++ get_token xi
get_token (',':xi) = [Token{ t = COMMA, literal = ","}] ++ get_token xi
get_token ('{':xi) = [Token{ t = RBRACE, literal = "{"}] ++ get_token xi
get_token ('}':xi) = [Token{ t = LBRACE, literal = "}"}] ++ get_token xi
get_token (' ':xi) = get_token xi
get_token ('\n':xi) = get_token xi
get_token ('\t':xi) = get_token xi
get_token "!" = [Token{ t = BANG, literal = "!"}]
get_token ('!':xi)
    | xi !! 0 == '=' = [Token{ t = NOT_EQUAL, literal = "!="}] ++ get_token xi
    | otherwise = get_token xi
get_token "=" = [Token{ t = ASSIGN, literal = "="}]
get_token ('=':xi)
    | xi !! 0 == '=' = [Token{ t = EQUAL, literal = "=="}] ++ get_token xi
    | otherwise = get_token xi
get_token (x:xi) = [Token{ t = NUMBER, literal = l }] ++ get_token ( drop (length l) (x:xi) )
    where
        l = case x of
           _ | elem x ['0'..'9'] -> get_numbers (x:xi)
             | elem x (['a'..'z'] ++ ['A'..'Z']) -> get_letters (x:xi)

get_token "" = [Token{ t = EOF, literal = ""}]
