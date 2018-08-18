
module Howard.REPL

import Control.Monad.State

import Howard.Expr
import Howard.Parser
import Howard.TypeChecker
import Howard.Interpreter

loop : StateT TopEnv IO ()
loop = do
    lift $ putStr "> "
    s <- lift getLine
    env <- get
    case parse s of
        Left  s => lift $ putStrLn $ "parse error"
        Right r => case r of
            Left st => case typeCheckStmt env st of
                Left te => lift $ putStrLn $ "type error: " ++ show te
                Right b => do
                    lift $ putStrLn $ show b
                    modify $ (::) b
            Right e => case typeCheckExpr env e of
                Left te => lift $ putStrLn $ "type error: " ++ show te
                Right t => lift $ do
                    putStr $ showP e
                    putStr " : "
                    printLn t
                    printLn . snd $ eval Root e
    loop

export
repl : IO ()
repl = do
    putStrLn "Howard interpreter."
    _ <- runStateT loop []
    pure ()
