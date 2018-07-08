
module Howard.TypeChecker

import Howard.Expr

export
TypeEnv : Type
TypeEnv = List (Name, TypE)

export
emptyTE : TypeEnv
emptyTE = []

equal : TypE -> TypE -> Bool
equal (Arr ax ay) (Arr bx by) = equal ax bx && equal ay by
equal TUnit TUnit = True
equal _ _         = False

export
typeCheck : TypeEnv -> Expr -> Either TypeError TypE
typeCheck te e = case e of
    EUnit   => Right TUnit
    Var s   => maybeToEither (UnboundVar e) $ lookup s te
    App a b => do
        ta <- typeCheck te a
        tb <- typeCheck te b
        case ta of
            Arr x y =>
                if equal x tb then Right tb
                              else Left $ Mismatch e x tb
            _ => Left $ Mismatch e (Arr tb TUnit) ta
    Abs (Prm s t) a => do
        ta <- typeCheck ((s, t) :: te) a
        Right $ Arr t ta
