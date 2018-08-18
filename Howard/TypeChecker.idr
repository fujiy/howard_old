
module Howard.TypeChecker

import Howard.Expr

public export
TopEnv : Type
TopEnv = List Binding

public export
TypeEnv : Type
TypeEnv = List (Name, TypE)

export
emptyTE : TypeEnv
emptyTE = []

equal : TypE -> TypE -> Bool
equal (Arr ax ay) (Arr bx by) = equal ax bx && equal ay by
equal TUnit TUnit = True
equal _ _         = False

typeCheck : TypeEnv -> Expr -> Either TypeError TypE
typeCheck te e = case e of
    EUnit   => pure TUnit
    Var s   => maybeToEither (UnboundVar e) $ lookup s te
    App a b => do
        ta <- typeCheck te a
        tb <- typeCheck te b
        case ta of
            Arr x y =>
                if equal x tb then pure tb
                              else Left $ Mismatch e x tb
            _ => Left $ Mismatch e (Arr tb TUnit) ta
    Abs (Prm s t) a => do
        ta <- typeCheck ((s, t) :: te) a
        pure $ Arr t ta
    Let (Bind s _ a) b => do
        ta <- typeCheck te a
        typeCheck ((s, ta) :: te) b

topBinds : TopEnv -> TypeEnv
topBinds = map $ \(Bind x t _) => (x, t)

export
typeCheckExpr : TopEnv -> Expr -> Either TypeError TypE
typeCheckExpr env = typeCheck (topBinds env)

export
typeCheckStmt : TopEnv -> Stmt -> Either TypeError Binding
typeCheckStmt env (TopBind (Bind x _ e)) = do
     t <- typeCheckExpr env e
     pure $ Bind x t e
