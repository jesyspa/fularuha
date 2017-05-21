{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Parser (
    Parser.parse
) where

import Text.Parsec as P
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Expr as Ex

import AST

oper :: Parsec String u Char
oper = oneOf "+-*/=<>!"

lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $ Tok.LanguageDef {
    Tok.commentStart = "{-",
    Tok.commentEnd = "-}",
    Tok.commentLine = "--",
    Tok.nestedComments = True,
    Tok.identStart = letter <|> char '$',
    Tok.identLetter = alphaNum <|> char '_',
    Tok.opStart = oper,
    Tok.opLetter = oper,
    Tok.reservedNames = ["let", "in", "case", "of", "print"],
    Tok.reservedOpNames = ["="],
    Tok.caseSensitive = True
}

parens = Tok.parens lexer
integer = Tok.integer lexer
semiSep = Tok.semiSep lexer
reservedOp = Tok.reservedOp lexer
reserved  = Tok.reserved lexer
identifier = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer

file :: Parsec String u FileAST
file = File <$ whitespace <*> semiSep decl
decl :: Parsec String u DeclAST
decl = Decl <$> var <*> many var <* reservedOp "=" <*> expr
expr :: Parsec String u ExprAST
expr = Ex.buildExpressionParser table exprPart
exprPart :: Parsec String u ExprAST
exprPart = parens expr
        <|> VarUse <$> var
        <|> Num <$> integer

table = [
        [Infix (whitespace >> return FunApl) AssocLeft]
    ]

var :: Parsec String u Var
var = identifier

parse :: String -> FileAST
parse s = case P.parse (file <* eof) "stdin" s of
    Left e -> error $ show e
    Right x -> x
