
module Howard.Parser

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.Combinators

import Howard.Expr


sign : List String -> Parser ()
sign = foldr (\s, p => skip (token s) <|>| p) (fail "empty list")

lambda : Parser ()
lambda = sign ["\\", "λ"]

expr : Parser Expr
term : Parser Expr
var : Parser String

expr = foldl1 App <$> some term

term = [| Var var |]
  <|>| [| Abs (lambda *> var) (dot *> expr) |]
  <|>| parens expr

var = pack <$> lexeme (some letter)

export
parse : String -> Either String Expr
parse = parseGeneric Nothing 4 (spaces *> expr)
