
module Howard.REPL

import Howard.Expr
import Howard.Parser
import Howard.TypeChecker
import Howard.Interpreter

loop : IO ()
loop = do
    putStr "> "
    s <- getLine
    case parse s of
        Left  s => putStrLn "parse error."
        Right e => case typeCheck emptyTE e of
            Left te => putStrLn $ "error: " ++ show te
            Right t => do
                putStr $ showP e
                putStr " : "
                printLn t
                printLn . snd $ eval Root e
    loop

export
repl : IO ()
repl = do
    putStrLn "Howard interpreter."
    loop
