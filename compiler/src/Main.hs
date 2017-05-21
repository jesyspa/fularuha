module Main where

import Parser (parse)
import Compile (compile)
import Assembly (ppASM)
import Text.PrettyPrint

main :: IO ()
main = do
    xs <- getContents
    let ast = parse xs
    let code = compile ast
    putStrLn . render . vcat . map ppASM $ code

