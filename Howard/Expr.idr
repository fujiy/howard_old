
module Howard.Expr


public export
Name : Type
Name = String

Expr : Type
Envir : Type


public export
data TypE = Arr TypE TypE
          | TUnit

public export
data Param = Prm Name TypE

public export
data Expr = Var Name
          | Abs Param Expr
          | App Expr Expr
          | Cls Envir Expr
          | EUnit

public export
data Envir = Root
           | Sub Name Expr Envir

apps : Expr -> List Expr
apps (App x y) = apps x ++ [y]
apps a         = [a]

export
showP : Expr -> String

showPT : TypE -> String

export
Show TypE where
    show (Arr x y) = showPT x ++ " → " ++ show y
    show TUnit     = "Unit"

export
Show Param where
  show (Prm s t) = s ++ ": " ++ showPT t

export
Show Expr where
    show (Var x)   = x
    show (App x y) = unwords $ map showP $ apps (App x y)
    show (Abs p y) = "λ" ++ show p ++ "." ++ show y
    show EUnit     = "unit"

showP (Var x) = x
showP EUnit   = "unit"
showP a       = showParens True $ show a

showPT TUnit = "Unit"
showPT a     = showParens True $ show a


public export
data TypeError = UnboundVar Expr
               | Mismatch Expr TypE TypE

export
Show TypeError where
    show (UnboundVar e)   = "Variable not in scope: " ++ show e
    show (Mismatch e x y) = "Couldn't match expected type " ++ show x ++ " with actual type " ++ show y
