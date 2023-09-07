module Parser where

import Data.Map (Map)-- for env
import qualified Data.Map as Map -- for env in tests


import Ast
import ParserMonad

-- reserved words
keywords = ["if","then","else", "let", "in", "true","false"]

-- make sure keywords not used as var
vars :: Parser Ast
vars = do s <- token $ varParser
          if s `elem` keywords
          then failParse
          else return $ Var s

parens :: Parser Ast
parens = do token $ literal "("
            ast <- parser
            token $ literal ")"
            return ast

ints :: Parser Ast
ints = token intParser
        `mapParser` (\ i -> ValInt i)

floats :: Parser Ast
floats = token notNatParser 
          `mapParser` (\f -> ValFloat f)

intsOrFloats :: Parser Ast
intsOrFloats = ints <|> floats

bools :: Parser Ast
bools = token boolParser
         `mapParser` (\b -> ValBool b)

nil :: Parser Ast
nil = parens +++ token(literal "[]") +++ parens
       `mapParser` (\ (_ , l) -> Nil)


cons :: Parser Ast
cons = parens +++ token(literal"[") +++ atoms +++ token(literal":") +++ atoms +++ token(literal"]")
        `mapParser` (\ ((((_ , l),_),r),_) -> Cons l r)      

atoms :: Parser Ast
atoms = ints <|> floats <|> negs <|> bools  <|>  nil  <|> parens <|> ifParser <|> letParser <|>  lambdaParser <|> vars

-- parsing arithmetic 
plus :: Parser Ast
plus = parens +++ intsOrFloats +++ token(literal "+") +++ intsOrFloats +++ parens
        `mapParser` (\ ((((_ , l),_),r),_) -> Plus l r)

minus :: Parser Ast
minus = parens +++ intsOrFloats +++ token(literal "-") +++ intsOrFloats +++ parens
         `mapParser` (\ ((((_ , l),_),r),_) -> Min l r)

mults :: Parser Ast
mults = parens +++ intsOrFloats +++ token(literal "*") +++ intsOrFloats +++ parens
         `mapParser` (\ ((((_ , l),_),r),_) -> Mult l r)

divsInt :: Parser Ast
divsInt = parens +++ ints +++ token(literal "//") +++ ints +++ parens
           `mapParser` (\ ((((_ , l),_),r),_) -> Div l r)

divsFloat :: Parser Ast
divsFloat = parens +++ floats +++ token(literal "/") +++ floats +++ parens
           `mapParser` (\ ((((_ , l),_),r),_) -> DivFloat l r)

divs :: Parser Ast
divs = divsInt <|> divsFloat

expsInt :: Parser Ast
expsInt = parens +++ ints +++ token(literal "**") +++ ints +++ parens
           `mapParser` (\ ((((_ , l),_),r),_) -> Exp l r)

expsFloat :: Parser Ast
expsFloat = parens +++ floats +++ token(literal "^") +++ floats +++ parens
             `mapParser` (\ ((((_ , l),_),r),_) -> ExpFloat l r)

exps :: Parser Ast
exps = expsInt <|> expsFloat


mods :: Parser Ast
mods = parens +++ ints +++ token(literal "%") +++ ints +++ parens
        `mapParser` (\ ((((_ , l),_),r),_) -> Mod l r)

negs :: Parser Ast
negs = parens +++ token(literal "-") +++ intsOrFloats +++ parens
        `mapParser` (\ ((_ , l),_) -> Neg l)

-- parsing logical operators
ands :: Parser Ast
ands = parens +++ bools +++ token(literal "&&") +++ bools +++ parens
        `mapParser` (\ ((((_ , l),_),r),_) -> And l r)

ors :: Parser Ast
ors = parens +++ bools +++ token(literal "||") +++ bools +++ parens
       `mapParser` (\ ((((_ , l),_),r),_) -> Or l r)

nots :: Parser Ast
nots = parens +++ token(literal "!") +++ bools +++ parens
        `mapParser` (\ ((_ , l),_) -> Not l)

intsOrFloatsOrBools :: Parser Ast
intsOrFloatsOrBools = intsOrFloats <|> bools

-- parsing relational operators
eqs :: Parser Ast
eqs = parens+++ intsOrFloatsOrBools +++ token(literal "==") +++ intsOrFloatsOrBools +++ parens
       `mapParser` (\ ((((_ , l),_),r),_) -> Eq l r)

neqs :: Parser Ast
neqs = parens +++ intsOrFloatsOrBools +++ token(literal "!=") +++ intsOrFloatsOrBools +++ parens
        `mapParser` (\ ((((_ , l),_),r),_) -> Neq l r)

lessThans :: Parser Ast
lessThans = parens +++ intsOrFloatsOrBools +++ token(literal "<") +++ intsOrFloatsOrBools +++ parens
             `mapParser` (\ ((((_ , l),_),r),_) -> LessThan l r)

ltOrEqs :: Parser Ast
ltOrEqs = parens +++ intsOrFloatsOrBools +++ token(literal "<=") +++ intsOrFloatsOrBools +++ parens
           `mapParser` (\ ((((_ , l),_),r),_) -> LtorEqto l r)

greaterThans :: Parser Ast
greaterThans = parens +++ intsOrFloatsOrBools +++ token(literal ">") +++ intsOrFloatsOrBools +++ parens
                `mapParser` (\ ((((_ , l),_),r),_) -> GreaterThan l r)

gtOrEqs :: Parser Ast
gtOrEqs = parens +++ intsOrFloatsOrBools +++ token(literal ">=") +++ intsOrFloatsOrBools +++ parens
           `mapParser` (\ ((((_ , l),_),r),_) -> GtorEqto l r)

relations :: Parser Ast
relations = eqs <|> neqs <|> lessThans <|> ltOrEqs <|> greaterThans <|> gtOrEqs

-- parsing expressions
lambdaParser :: Parser Ast
lambdaParser = parens +++ token(literal "\\") +++ vars +++ token(literal "->") +++ apps
                `mapParser` (\ ((((_ , l),_),_),r) -> Lam l r)

ifParser :: Parser Ast
ifParser = parens +++ token(literal "if") +++ intsOrFloatsOrBools +++ token(literal "then") +++ intsOrFloatsOrBools +++ token(literal "else")
           +++ intsOrFloatsOrBools `mapParser` (\((((((x,_), _),_),y),_),z) -> If x y z)

letParser :: Parser Ast
letParser = parens +++ token(literal "let") +++ vars +++ token(literal"=") +++ atoms +++ token(literal"in") +++ atoms
             `mapParser` (\((((((_,x), _),_),y),_),z) -> Let x y z)

apps :: Parser Ast
apps = withInfix undefined [("",App)] -- the tokens eat up all the spaces so we split on the empty string

multsExpDiv :: Parser Ast
multsExpDiv = exps <|> mults <|> divs

arithmetics :: Parser Ast
arithmetics = plus <|> minus 

-- | parser for the language
parser :: Parser Ast
parser = atoms <|> multsExpDiv <|> arithmetics <|> cons <|> apps <|> relations
