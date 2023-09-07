module CheckTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Check

-- provide tests that show your check works

runEmptyT :: Ast -> Unsafe TestVal 
runEmptyT exp = 
  case runEnvUnsafe (eval exp) Map.empty of 
    Ok res -> Ok $ valToTestVal res 
    Error msg -> Error msg
    
tests = testGroup "CheckTest" 
  [
  testCase "Free var test examples" $
  	do
  		assertBool "\x -> xy" isError (runEmptyT $ (Lam X (Mult X Y)))
  		assertBool  "\x -> \y -> xyz" isError (runEmptyT $ (Lam X (Lam Y (Mult X (Mult Y Z)))))
  		assertBool "\y -> y+x" isError (runEmptyT $ (Lam Y (Plus X Y)))
  		assertBool "x -> x > y" isError (runEmptyT $ (Lam X (GreaterThan X Y))) 
  		assertBool "x -> x + -y" isError (runEmptyT $ (Lam X (Min X (-Y))))


  -- ...
  ]

