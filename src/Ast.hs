module Ast where

import HelpShow

data Ast = ValBool Bool
         | And Ast Ast | Or Ast Ast | Not Ast
         | Eq Ast Ast | Neq Ast Ast | LessThan Ast Ast | LtorEqto Ast Ast | GreaterThan Ast Ast | GtorEqto Ast Ast

         | ValInt Int
         | Plus Ast Ast | Min Ast Ast | Mult Ast Ast | Div Ast Ast| Exp Ast Ast| Mod Ast Ast | Neg Ast


         | Nil
         | Cons Ast Ast
         | ListConcat Ast Ast
         | IndexOp Ast Ast

         | If Ast Ast Ast
         | Let String Ast Ast

         | Var String
         | Lam String Ast
         | App Ast Ast

         | ValString String
         | ValChar Char

         | Separator Ast Ast

         | ValFloat Float
         | DivFloat Ast Ast| ExpFloat Ast Ast | PlusFloat Ast Ast | MinFloat Ast Ast | MultFloat Ast Ast | NegFloat Ast
         | LessThanF Ast Ast | LtorEqtoF Ast Ast | GreaterThanF Ast Ast | GtorEqtoF Ast Ast

         | Print Ast
--           deriving (Eq,Show) -- helpful to use this during testing
         deriving Eq

instance Show Ast where
  show ast = showPretty ast 0

-- | output the fully parenthesized statement
showFullyParen :: Ast -> String  
-- Bool
showFullyParen (ValBool True) = "(" ++ "true" ++ ")"
showFullyParen (ValBool False) = "(" ++ "false" ++ ")"
showFullyParen (And l r) = "(" ++ (showFullyParen l) ++ " && " ++ (showFullyParen r) ++ ")"
showFullyParen (Or l r) = "(" ++ (showFullyParen l) ++ " || " ++ (showFullyParen r) ++ ")"
showFullyParen (Not a) = "(" ++ " ! " ++ (showFullyParen a) ++ ")"
showFullyParen (Eq l r) = "(" ++ (showFullyParen l) ++ " == " ++ (showFullyParen r) ++ ")"
showFullyParen (Neq l r) = "(" ++ (showFullyParen l) ++ " != " ++ (showFullyParen r) ++ ")"
showFullyParen (LessThan l r) = "(" ++ (showFullyParen l) ++ " < " ++ (showFullyParen r) ++ ")"
showFullyParen (LtorEqto l r) = "(" ++ (showFullyParen l) ++ " <= " ++ (showFullyParen r) ++ ")"
showFullyParen (GreaterThan l r) = "(" ++ (showFullyParen l) ++ " > " ++ (showFullyParen r) ++ ")"
showFullyParen (GtorEqto l r) = "(" ++ (showFullyParen l) ++ " >= " ++ (showFullyParen r) ++ ")" 

-- Int
showFullyParen (ValInt i) = "(" ++ show i ++ ")"
showFullyParen (Plus l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (Min l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (Mult l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (Div l r) = "(" ++ (showFullyParen l) ++ " // " ++ (showFullyParen r) ++ ")"
showFullyParen (Exp l r) = "(" ++ (showFullyParen l) ++ " ** " ++ (showFullyParen r) ++ ")"
showFullyParen (Mod l r) = "(" ++ (showFullyParen l) ++ " % " ++ (showFullyParen r) ++ ")"
showFullyParen (Neg i) = "(" ++ " - " ++ (showFullyParen i) ++ ")"

-- Float
showFullyParen (ValFloat f) = "(" ++ show f ++ ")"
showFullyParen (PlusFloat l r) = "(" ++ (showFullyParen l) ++ " + " ++ (showFullyParen r) ++ ")"
showFullyParen (MinFloat l r) = "(" ++ (showFullyParen l) ++ " - " ++ (showFullyParen r) ++ ")"
showFullyParen (DivFloat l r) = "(" ++ (showFullyParen l) ++ " / " ++ (showFullyParen r) ++ ")"
showFullyParen (ExpFloat l r) = "(" ++ (showFullyParen l) ++ " ^ " ++ (showFullyParen r) ++ ")"
showFullyParen (MultFloat l r) = "(" ++ (showFullyParen l) ++ " * " ++ (showFullyParen r) ++ ")"
showFullyParen (NegFloat f) = "(" ++ " - " ++ (showFullyParen f) ++ ")"
showFullyParen (LessThanF l r) = "(" ++ (showFullyParen l) ++ " < " ++ (showFullyParen r) ++ ")"
showFullyParen (LtorEqtoF l r) = "(" ++ (showFullyParen l) ++ " <= " ++ (showFullyParen r) ++ ")"
showFullyParen (GreaterThanF l r) = "(" ++ (showFullyParen l) ++ " > " ++ (showFullyParen r) ++ ")"
showFullyParen (GtorEqtoF l r) = "(" ++ (showFullyParen l) ++ " >= " ++ (showFullyParen r) ++ ")" 


-- Expressions 
showFullyParen (If b t e) = "(if " ++ (showFullyParen b) ++ " then " ++ (showFullyParen t) ++ " else " ++ (showFullyParen e) ++ ")"
showFullyParen (Let v a bod) = "(let " ++ v ++ " = " ++ (showFullyParen a) ++ " in " ++ (showFullyParen bod) ++ ")"
showFullyParen (Lam v bod) = "(\\" ++ v ++ " -> " ++ (showFullyParen bod) ++ ")"
showFullyParen (App f a) = "( " ++ (showFullyParen f)  ++ " " ++ (showFullyParen a) ++ ")"
showFullyParen (Var s) = "( " ++ s ++ ")"
showFullyParen (Separator l r) = "(" ++ (showFullyParen l) ++ "; " ++ (showFullyParen r) ++ ")"
showFullyParen  (Print a) = "print(" ++ (showFullyParen a) ++ ")"

-- Lists
showFullyParen (Cons h t) = "(" ++ (showFullyParen h)  ++ " : " ++ (showFullyParen t) ++ ")"
showFullyParen  Nil = "( [] )"
showFullyParen (ListConcat l r) = "(" ++ (showFullyParen l) ++ " ++ " ++ (showFullyParen r) ++ ")"
showFullyParen (IndexOp a i) = "(" ++ (showFullyParen a) ++ " !! " ++ (showFullyParen i) ++ ")"

-- Strings and Chars
showFullyParen (ValString s) = "(" ++  s ++ ")"
showFullyParen (ValChar c) = "(" ++  [c] ++  ")"

-- | provide a nice show with minimal parentheses
showPretty :: Ast  -- ^ The Ast to show
            -> Integer  -- ^ The precedence of the root expression, see the doc for 'HelpShow.parenthesize' for more detail
            -> String  -- ^ the minimally parenthesized string representing the input Ast
-- Atomic Expressions and Functions (High precedence)
showPretty (ValInt i) _ =  if i < 0
              then  "(" ++ show i ++ ")"
              else show i
showPretty (ValFloat f) _ =  if f < 0
              then  "(" ++ show f ++ ")"
              else show f             
showPretty (ValBool True) _ =  "true"
showPretty (ValBool False)  _  = "false"
showPretty (ValString s) _ = s
showPretty (ValChar c) _ = [c]
showPretty Nil _ = "[]"
showPretty (Var s) _ = s
showPretty (Lam v bod) i = parenthesize 1 i $ "\\ " ++ v ++ " -> " ++ (showPretty bod 1)
showPretty (Let v a bod)  i = parenthesize 1 i $  "let " ++ v ++ " = " ++ (showPretty a 1) ++ " in " ++ (showPretty bod 1)
showPretty (If b t e) i = parenthesize 1 i $  "if " ++ (showPretty b 1) ++ " then " ++ (showPretty t 1) ++ " else " ++ (showPretty e 1)

-- Negation, Unary Minus, Print (Second Highest Precendece)
showPretty (Not a) i = parenthesize 2 i $ " ! " ++ (showPretty a 2)
showPretty (Neg a) i = parenthesize 2 i $ " - " ++ (showPretty a 2)
showPretty (NegFloat a) i = parenthesize 2 i $ " - " ++ (showPretty a 2)
showPretty (Print a) i = parenthesize 2 i $ "print(" ++ (showPretty a 2) ++ ")"

-- Index operator 
showPretty (IndexOp l r) i = parenthesize 3 i $ (showPretty l 3) ++ " !! " ++ (showPretty r 4)

-- Exp then Mult, Div, Mod
showPretty (Exp l r) i = parenthesize 5 i $ (showPretty l 5) ++ " ** " ++ (showPretty r 6)
showPretty (ExpFloat l r) i = parenthesize 5 i $ (showPretty l 5) ++ " ^ " ++ (showPretty r 6)
showPretty (Mult l r) i = parenthesize 7 i $ (showPretty l 7) ++ " * " ++ (showPretty r 8)
showPretty (MultFloat l r) i = parenthesize 7 i $ (showPretty l 7) ++ " * " ++ (showPretty r 8)
showPretty (Div l r) i = parenthesize 7 i $ (showPretty l 7) ++ " // " ++ (showPretty r 8)
showPretty (DivFloat l r) i = parenthesize 7 i $ (showPretty l 7) ++ " / " ++ (showPretty r 8)
showPretty (Mod l r) i = parenthesize 7 i $ (showPretty l 7) ++ " % " ++ (showPretty r 8)

-- Arithmetic Expressions
showPretty (Min l r) i = parenthesize 9 i $ (showPretty l 9) ++ " - " ++ (showPretty r 10)
showPretty (MinFloat l r) i = parenthesize 9 i $ (showPretty l 9) ++ " - " ++ (showPretty r 10)
showPretty (Plus l r) i = parenthesize 9 i $ (showPretty l 9) ++ " + " ++ (showPretty r 10)
showPretty (PlusFloat l r) i = parenthesize 9 i $ (showPretty l 9) ++ " + " ++ (showPretty r 10)

-- Lists 
showPretty (Cons l r) i = parenthesize 11 i $ (showPretty l 12) ++ " : " ++ (showPretty r 11)
showPretty (ListConcat l r) i = parenthesize 11 i $ (showPretty l 11) ++ " ++ " ++ (showPretty r 12)

-- Relational Operators
showPretty (Eq l r) i = parenthesize 13 i $ (showPretty l 13) ++ " == " ++ (showPretty r 14)
showPretty (Neq l r) i = parenthesize 13 i $ (showPretty l 13) ++ " != " ++ (showPretty r 14)
showPretty (LessThan l r) i = parenthesize 13 i $ (showPretty l 13) ++ " < " ++ (showPretty r 14)
showPretty (LessThanF l r) i = parenthesize 13 i $ (showPretty l 13) ++ " < " ++ (showPretty r 14)
showPretty (LtorEqto l r) i = parenthesize 13 i $ (showPretty l 13) ++ " <= " ++ (showPretty r 14)
showPretty (LtorEqtoF l r) i = parenthesize 13 i $ (showPretty l 13) ++ " <= " ++ (showPretty r 14)
showPretty (GreaterThan l r) i = parenthesize 13 i $ (showPretty l 13) ++ " > " ++ (showPretty r 14)
showPretty (GreaterThanF l r) i = parenthesize 13 i $ (showPretty l 13) ++ " > " ++ (showPretty r 14)
showPretty (GtorEqto l r) i = parenthesize 13 i $ (showPretty l 13) ++ " >= " ++ (showPretty r 14)
showPretty (GtorEqtoF l r) i = parenthesize 13 i $ (showPretty l 13) ++ " >= " ++ (showPretty r 14)

-- Logical Operators
showPretty (Or l r) i = parenthesize 15 i $ (showPretty l 15) ++ " || " ++ (showPretty r 16)
showPretty (And l r) i = parenthesize 17 i $ (showPretty l 17) ++ " && " ++ (showPretty r 18)

-- Function App after And Operator
showPretty (App l r) i = parenthesize 19 i $ (showPretty l 19) ++ " " ++ (showPretty r 20)

-- Separator (Lowest Precedence)
showPretty (Separator l r) i = parenthesize 21 i $ (showPretty l 21) ++ "; " ++ (showPretty r 22)
