module ParserTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
import Test.Tasty.QuickCheck 

import ParserMonad (parse)

import Ast (showFullyParen, showPretty, Ast(..))
import Parser (parser)

-- provide tests that show your parser works

instance Arbitrary Ast where
  arbitrary = sized arbitrarySizedAst

arbitrarySizedAst ::  Int -> Gen Ast
arbitrarySizedAst m | m < 1 = do i <- arbitrary
                                  b <- arbitrary
                                  node <- elements [ValInt i, ValBool b, , ValFloat f, ValString s, ValChar c, Nil]
                                  return $ node
arbitrarySizedAst m | otherwise = do l <- arbitrarySizedAst (m `div` 2)
                                     r <- arbitrarySizedAst (m `div` 2)
                                     str <- elements ["x","y","z"]
                                     ifast <- arbitrarySizedIf m
                                     node <- elements [And l r, Or l r, Not l, Eq l r, Neq l r, 
                                                        LessThan l r, LtorEqto l r, GreaterThan l r, GtorEqto l r, 
                                                        Plus l r, Min l r, Mult l r, Div l r, Exp l r, Mod l r, Neg l,
                                                        DivFloat l r, ExpFloat l r,
                                                        Cons l r, ListConcat l r, IndexOp l r,
                                                        ifast,
                                                        Let str l r,
                                                        Lam str l,
                                                        App l r,
                                                        Var str,
                                                        Separator l r,
                                                        Print l
                                                      ]
                                      return node
  
-- it would be better if every branch were built like this so the balance would be maintained
arbitrarySizedIf ::  Int -> Gen Ast
arbitrarySizedIf m = do b <- arbitrarySizedAst (m `div` 3)
                        t <- arbitrarySizedAst (m `div` 3)
                        e <- arbitrarySizedAst (m `div` 3)
                        return $ If b t e

parserTest = testGroup
      "ParserTest"
      [
      testProperty "parse should return the same AST when fully parenthisized" $ ((\ x -> Just (x , "") == (parse parser $ showFullyParen x)) :: Ast -> Bool),
      testProperty "parse should return the same AST when pretty printed" $ ((\ x -> Just (x , "") == (parse parser $ showPretty x 0)) :: Ast -> Bool)
      ]

