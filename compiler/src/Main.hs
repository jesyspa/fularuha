module Main where

import Parser (parse)

main :: IO ()
main = do
    xs <- getContents
    let ast = parse xs
    print ast

