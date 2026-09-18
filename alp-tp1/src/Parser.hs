module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","

                        -- Ejercicio 3
                        , "++"
                        , "--"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = chainl1 intterm addop

addop :: Parser (Exp Int -> Exp Int -> Exp Int)
addop = (reservedOp lis "+" >> return Plus)
  <|> (reservedOp lis "-" >> return Minus)

-- Ejercicio 3

intterm :: Parser (Exp Int)
intterm = chainl1 intfactor multop

multop :: Parser (Exp Int -> Exp Int -> Exp Int)
multop = (reservedOp lis "*" >> return Times)
  <|> (reservedOp lis "/" >> return Div)

intfactor :: Parser (Exp Int)
intfactor = 
  do reservedOp lis "-"
     fac <- intfactor
     return (UMinus fac)  
  <|> 
  do n <- natural lis
     return (Const (fromInteger n)) 
  <|> 
  parens lis intexp
  <|>
  do v <- identifier lis
     (reservedOp lis "++" >> return (VarInc v)) -- O es '++'
       <|> (reservedOp lis "--" >> return (VarDec v)) -- O es '--'
       <|> return (Var v) -- O era una variable simplemente


------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 booltermand orop

orop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orop =reservedOp lis "||" >> return Or

booltermand :: Parser (Exp Bool)
booltermand = chainl1 booltermnot andop

andop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andop = reservedOp lis "&&" >> return And

booltermnot :: Parser (Exp Bool)
booltermnot = 
  (do reservedOp lis "!"
      b <- booltermnot
      return (Not b))
  <|> boolfactor


boolfactor :: Parser (Exp Bool)
boolfactor = 
  try (do a <- intexp
          op <- 
            (reservedOp lis "<" >> return Lt)
            <|>
            (reservedOp lis ">" >> return Gt)
            <|>
            (reservedOp lis "==" >> return Eq)
            <|>
            (reservedOp lis "!=" >> return NEq)
          b <- intexp
          return (op a b))
  <|>
    (reserved lis "true" >> return BTrue)
  <|>
    (reserved lis "false" >> return BFalse)
  <|>
    parens lis boolexp
  

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 commfactor comaop

comaop :: Parser (Comm -> Comm -> Comm)
comaop = reservedOp lis ";" >> return Seq

commfactor :: Parser Comm
commfactor =
  (do x <- identifier lis
      reservedOp lis "="
      n <- intexp
      return (Let x n))
  <|>
  (reserved lis "skip" >> return Skip)
  <|>
  try (do
        reserved lis "if"
        cond <- boolexp
        -- No tiene then, usa braces
        caso1 <- braces lis comm
        reserved lis "else"
        caso2 <- braces lis comm
        return (IfThenElse cond caso1 caso2))
  <|>
  (do
    reserved lis "if"
    cond <- boolexp
    caso1 <- braces lis comm
    return (IfThenElse cond caso1 Skip))
  <|>
  (do
    reserved lis "repeat"
    body <- braces lis comm
    reserved lis "until"
    cond <- boolexp
    return (RepeatUntil body cond))
        
------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
