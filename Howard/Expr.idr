
module Howard.Expr


public export
Name : Type
Name = String

Expr : Type
Envir : Type

public export
data Expr = Var Name
          | Abs Name Expr
          | App Expr Expr
          | Cls Envir Expr

public export
data Envir = Root
           | Sub Name Expr Envir

apps : Expr -> List Expr
apps (App x y) = apps x ++ [y]
apps a         = [a]

showP : Expr -> String

export
Show Expr where
    show (Var x)   = x
    show (App x y) = unwords $ map showP $ apps (App x y)
    show (Abs x y) = "λ" ++ x ++ "." ++ show y

showP (Var x) = x
showP a       = showParens True $ show a
