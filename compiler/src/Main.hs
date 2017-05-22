module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Parser (parse)
import Compile (compile)
import Assembly (ppASMs)
import Text.PrettyPrint (render)

data Config = Config
    { inFilePath :: FilePath
    , outFilePath :: FilePath
    }
    deriving (Read, Show)

argCfg :: Parser Config
argCfg = Config
    <$> strOption (long "input" <> short 'i' <> metavar "INPUT")
    <*> (strOption (long "output" <> short 'o' <> metavar "OUTPUT") <|> pure "output.flbc")

argParser :: ParserInfo Config
argParser = info argCfg (fullDesc <> header "FuLaRuHa compiler")

main :: IO ()
main = do
    cfg <- execParser argParser
    xs <- readFile (inFilePath cfg)
    let ast = parse xs
    let code = compile ast
    let output = render . ppASMs $ code
    writeFile (outFilePath cfg) output

