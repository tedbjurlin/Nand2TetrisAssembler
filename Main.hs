{-# Language GADTSyntax #-}

module Main where

import           Data.Void ( Void )
import           System.Environment ( getArgs )
import           System.FilePath.Posix (takeBaseName )
import Text.Megaparsec as P
    ( parse, between, choice, many, eof, Parsec, try, (<|>), notFollowedBy, errorBundlePretty, empty )
import Text.Megaparsec.Char
    ( string, char, digitChar, letterChar, space1 )
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Printf
import           Numeric ( showIntAtBase )
import           Data.Char ( intToDigit )
import           Data.Map ( Map )
import qualified Data.Map as M

type Parser = Parsec Void String

type Vars = Map String String

data Exp where
  CExp  :: String -> Exp
  AExp  :: AExpression -> Exp
  Label :: String -> Exp
  deriving (Show, Eq)

data AExpression where
  Var :: String -> AExpression
  Lab :: String -> AExpression
  deriving (Show, Eq)

type Program = [Exp]

whitespace :: Parser ()
whitespace = L.space
  space1 
  (L.skipLineComment "//")
  P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

reserved :: String -> Parser String
reserved = lexeme . P.try . string

pPredef :: Parser String
pPredef = choice
  [ "0"  <$ reserved "R0"
  , "0"  <$ reserved "SP"
  , "10" <$ reserved "R10"
  , "11" <$ reserved "R11"
  , "12" <$ reserved "R12"
  , "13" <$ reserved "R13"
  , "14" <$ reserved "R14"
  , "15" <$ reserved "R15"
  , "1"  <$ reserved "R1"
  , "1"  <$ reserved "LCL"
  , "2"  <$ reserved "R2"
  , "2"  <$ reserved "ARG"
  , "3"  <$ reserved "R3"
  , "3"  <$ reserved "THIS"
  , "4"  <$ reserved "R4"
  , "4"  <$ reserved "THAT"
  , "5"  <$ reserved "R5"
  , "6"  <$ reserved "R6"
  , "7"  <$ reserved "R7"
  , "8"  <$ reserved "R8"
  , "9"  <$ reserved "R9"
  , "16384" <$ reserved "SCREEN"
  , "24576" <$ reserved "KBD" ]

symbol :: Parser String
symbol = lexeme partSymbol
  
partSymbol :: Parser String
partSymbol = do
  a <- choice
    [ letterChar
    , char '_'
    , char '.'
    , char '$'
    , char ':' ]
  rest <-  many $ choice
    [ letterChar
    , digitChar
    , char '_'
    , char '.'
    , char '$'
    , char ':' ]
  return $ a:rest

sToken :: String -> Parser String
sToken = L.symbol whitespace

integer :: Parser String
integer = show <$> (lexeme L.decimal :: Parser Integer)

parens :: Parser a -> Parser a
parens = between (sToken "(") (sToken ")")

pAExp :: Parser Exp
pAExp = AExp <$> (sToken "@" *> pAValue)

pAValue :: Parser AExpression
pAValue = choice
  [ Var <$> pPredef
  , Var <$> integer
  , Lab <$> symbol ]

pCExp :: Parser Exp
pCExp = CExp <$> ((\d c j -> "111" ++ c ++ d ++ j)
  <$> choice [ "000" <$ notFollowedBy pDest, pDest ]
  <*> pComp
  <*> choice [ "000" <$ notFollowedBy pJump, pJump])

pComp :: Parser String
pComp = choice
  [ "0101010" <$ reserved "0"
  , "0111111" <$ reserved "1"
  , "0111010" <$ reserved "-1"
  , "0001101" <$ reserved "!D"
  , "0110001" <$ reserved "!A"
  , "1110001" <$ reserved "!M"
  , "0001111" <$ reserved "-D"
  , "0110011" <$ reserved "-A"
  , "1110011" <$ reserved "-M"
  , "0011111" <$ reserved "D+1"
  , "0110111" <$ reserved "A+1"
  , "1110111" <$ reserved "M+1"
  , "0001110" <$ reserved "D-1"
  , "0110010" <$ reserved "A-1"
  , "1110010" <$ reserved "M-1"
  , "0000010" <$ reserved "D+A"
  , "1000010" <$ reserved "D+M"
  , "0010011" <$ reserved "D-A"
  , "1010011" <$ reserved "D-M"
  , "0000111" <$ reserved "A-D"
  , "1000111" <$ reserved "M-D"
  , "0000000" <$ reserved "D&A"
  , "1000000" <$ reserved "D&M"
  , "0010101" <$ reserved "D|A"
  , "1010101" <$ reserved "D|M"
  , "0001100" <$ reserved "D"
  , "0110000" <$ reserved "A"
  , "1110000" <$ reserved "M" ]

pDest :: Parser String
pDest = choice
  [ "111" <$ reserved "ADM" <* whitespace <* sToken "="
  , "111" <$ reserved "AMD" <* whitespace <* sToken "="
  , "111" <$ reserved "DAM" <* whitespace <* sToken "="
  , "111" <$ reserved "DMA" <* whitespace <* sToken "="
  , "111" <$ reserved "MAD" <* whitespace <* sToken "="
  , "111" <$ reserved "MDA" <* whitespace <* sToken "="
  , "011" <$ reserved "DM" <* whitespace <* sToken "="
  , "011" <$ reserved "MD" <* whitespace <* sToken "="
  , "101" <$ reserved "AM" <* whitespace <* sToken "="
  , "101" <$ reserved "MA" <* whitespace <* sToken "="
  , "110" <$ reserved "AD" <* whitespace <* sToken "="
  , "110" <$ reserved "DA" <* whitespace <* sToken "="
  , "010" <$ reserved "D" <* whitespace <* sToken "="
  , "100" <$ reserved "A" <* whitespace <* sToken "="
  , "001" <$ reserved "M" <* whitespace <* sToken "=" ]

pJump :: Parser String
pJump = choice
  [ "001" <$ P.try (sToken ";" *> whitespace *> reserved "JGT")
  , "010" <$ P.try (sToken ";" *> whitespace *> reserved "JEQ")
  , "011" <$ P.try (sToken ";" *> whitespace *> reserved "JGE")
  , "100" <$ P.try (sToken ";" *> whitespace *> reserved "JLT")
  , "101" <$ P.try (sToken ";" *> whitespace *> reserved "JNE")
  , "110" <$ P.try (sToken ";" *> whitespace *> reserved "JLE")
  , "111" <$ P.try (sToken ";" *> whitespace *> reserved "JMP") ]

pLabel :: Parser Exp
pLabel = Label <$> parens symbol

pExp :: Parser Exp
pExp = P.try pAExp <|> P.try pCExp <|> pLabel

pProgram :: Parser Program
pProgram = whitespace *> many pExp <* eof

idLabels :: Program -> Integer -> (Vars, [String])
idLabels [] _           = (M.empty, [])
idLabels (Label s:ls) i = let (v, vs) = idLabels ls i in
  (M.insert s (show i) v, vs)
idLabels (AExp (Lab s):ls) i = let (v, vs) = idLabels ls (i+1) in
  (v, s:vs)
idLabels (_:ls) i = idLabels ls (i+1)

checkValues :: (Vars, [String]) -> Integer -> Vars
checkValues (v, []) _   = v
checkValues (v, s:ls) i = case M.lookup s v of
  Nothing -> checkValues (M.insert s (show i) v, ls) (i+1)
  Just _  -> checkValues (v, ls) i

construct :: Vars -> Program -> [String]
construct _ [] = []
construct v (CExp s:ls) = s:construct v ls
construct v (AExp (Var s):ls) =
  printf "%016s" (showIntAtBase 2 intToDigit (read s :: Integer) ""):construct v ls
construct v (AExp (Lab s):ls) = case M.lookup s v of
  Nothing -> construct v ls
  Just p  ->
    printf "%016s" (showIntAtBase 2 intToDigit (read p :: Integer) ""):construct v ls
construct v (_:ls) = construct v ls


main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  let program = assemble contents in
    writeFile (outputFileName $ head args) (unlines $ construct (checkValues (idLabels program 0) 16) program)

outputFileName :: String -> String
outputFileName file = takeBaseName file ++ ".hack"

assemble :: String -> Program
assemble s = case parse pProgram "" s of
  Left err -> error (errorBundlePretty err)
  Right p  -> p
