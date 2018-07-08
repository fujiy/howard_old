
module Howard.Interpreter

import Howard.Expr

%hide Interfaces.Abs

lookup : Name -> Envir -> Maybe Expr
lookup x Root        = Nothing
lookup x (Sub y e p) = if x == y then Just e else lookup x p

export
eval : Envir -> Expr -> (Envir, Expr)
eval env e = case e of
    Var s   => maybe (env, e) (eval env) $ lookup s env
    App a b => case eval env a of
        (env', Abs (Prm s _) c) => eval (Sub s (Cls env b) env') c
        (env', a')              => (env, App (Cls env' a') b)
    Cls env' a => eval env' a
    _       => (env, e)
