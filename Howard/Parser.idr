
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

-- Hi. I'm trying lightyear, but there no operator (<&) in Prelude.Functor like Haskell...Is it better if I add it?
-- (<$) : Functor f => a -> f b -> f a. I want to write not `f <$> (m_ignore *> ma) <*> mb` but `f <$ m_ignore <*> ma <*> mb` in Applicative style.
-- I think Control.Applicative is better, too.
-- In terms of laziness, I'll look into implementation of similar functions like (<*).
-- I understand.
-- it's complicated
-- For now following current Applicative, it should be strict just using `const`, isn't it?
-- Imprementation of (<*) seems strict, just using `const`.
