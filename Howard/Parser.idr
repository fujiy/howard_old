
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

arrow : Parser ()
arrow = sign ["->", "→"]

expr  : Parser Expr
term  : Parser Expr
var   : Parser String
param : Parser Param
type  : Parser TypE
typet : Parser TypE

expr = foldl1 App <$> some term

term = [| (const EUnit) (token "unit") |]
  <|>| [| Var var |]
  <|>| [| Abs (lambda *> param) (dot *> expr) |]
  <|>| parens expr

var = pack <$> lexeme (some letter)

param = [| Prm var (colon *> type) |]
   <|>| parens param

type = foldr1 Arr <$> sepBy1 typet arrow

typet = [| (const TUnit) (token "Unit") |]
   <|>| parens type

export
parse : String -> Either String Expr
parse = parseGeneric Nothing 4 (spaces *> expr)
