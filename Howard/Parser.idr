
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

darrow : Parser ()
darrow = sign ["=>", "⇒"]

coloneq : Parser ()
coloneq = sign [":=", "≔"]

letsign : Parser ()
letsign = sign ["let"]

insign : Parser ()
insign = sign ["in"]

keywords : List Name
keywords = ["\\", "λ", "->", "→", "=>", "⇒", ":=", "≔", "let", "in", ":"]

type  : Parser TypE
typet : Parser TypE

type = foldr1 Arr <$> sepBy1 typet arrow

typet = [| (const TUnit) (token "Unit") |]
   <|>| parens type


expr  : Parser Expr
term  : Parser Expr
var   : Parser String
param : Parser Param
bind  : Parser Binding
stmt  : Parser Stmt

expr = foldl1 App <$> some term

term = [| (const EUnit) (token "unit") |]
  <|>| [| Var var |]
  <|>| [| Abs (lambda *> param) (darrow *> expr) |]
  <|>| [| Let (letsign *> bind) (insign *> expr) |]
  <|>| parens expr

var = pack <$> lexeme (some letter)
  >>= \x => if elem x keywords then fail "keyword" else pure x

param = [| Prm var (colon *> type) |]
   <|>| parens param

bind = [| Bind var (pure Unknown) (coloneq *> expr) |]

stmt = [| TopBind bind |]

export
parse : String -> Either String (Either Stmt Expr)
parse = parseGeneric Nothing 4 $
    spaces *> ([| Left (stmt <* eof) |] <|>| [| Right (expr <* eof) |])
