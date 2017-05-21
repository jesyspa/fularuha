module Main where

import Parser (parse)
import Compile (compile)
import Assembly (ppASM)
import Text.PrettyPrint

main :: IO ()
main = do
    xs <- getContents
    let ast = parse xs
    print ast
    let asm = compile ast
    print asm
    putStrLn . render . vcat $ map ppASM asm

