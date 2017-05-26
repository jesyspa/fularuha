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
    Tok.reservedNames = ["let", "in", "case", "of", "true", "false", "if", "then", "else", "primitive", "data"],
    Tok.reservedOpNames = ["=", "==", "*", "+", "-", "<"],
    Tok.caseSensitive = True
}

parens = Tok.parens lexer
natural = Tok.natural lexer
semiSep = Tok.semiSep lexer
reservedOp = Tok.reservedOp lexer
reserved  = Tok.reserved lexer
identifier = Tok.identifier lexer
whitespace = Tok.whiteSpace lexer

file :: Parsec String u FileAST
file = File <$ whitespace <*> semiSep decl
decl :: Parsec String u DeclAST
decl = Decl <$> var <*> many var <* reservedOp "=" <*> expr
    <|> Prim <$ reserved "primitive" <*> var
    <|> Data <$ reserved "data" <*> identifier <* reservedOp "=" <*> sepBy1 constructor (reservedOp "|")

constructor :: Parsec String u ConsAST
constructor = Constructor <$> identifier <*> natural

expr :: Parsec String u ExprAST
expr = Ex.buildExpressionParser table exprSeq
exprSeq :: Parsec String u ExprAST
exprSeq = chainl1 exprPart (return FunApp)
exprPart :: Parsec String u ExprAST
exprPart = parens expr
        <|> VarUse <$> var
        <|> reserved "true" *> return (Bool True)
        <|> reserved "false" *> return (Bool False)
        <|> (\x y z -> FunApp (FunApp (FunApp (VarUse "$branch") x) y) z)
            <$ reserved "if"
            <*> expr
            <* reserved "then"
            <*> expr
            <* reserved "else"
            <*> expr
        <|> Num <$> natural

table = [ [ Infix (opParser "*" "$mul") AssocLeft]
        , [ Infix (opParser "+" "$add") AssocLeft
          , Infix (opParser "-" "$sub") AssocLeft]
        , [ Infix (opParser "==" "$equal") AssocNone
          , Infix (opParser "<" "$less_than") AssocNone]]
    where funAppl2 c x y = FunApp (FunApp (VarUse c) x) y
          opParser sym name = reservedOp sym >> return (funAppl2 name)

var :: Parsec String u Var
var = identifier

parse :: String -> FileAST
parse s = case P.parse (file <* eof) "stdin" s of
    Left e -> error $ show e
    Right x -> x
