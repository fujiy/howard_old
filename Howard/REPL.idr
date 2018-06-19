
module Howard.REPL

import Howard.Expr
import Howard.Parser
import Howard.Interpreter

loop : IO ()
loop = do
    putStr "> "
    s <- getLine
    case parse s of
        Left  s => putStrLn s
        Right e => do
            printLn e
            printLn . snd $ eval Root e
    loop

export
repl : IO ()
repl = do
    putStrLn "Howard interpreter."
    loop
