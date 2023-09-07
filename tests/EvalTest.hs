module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 
import qualified Data.Map as Map

import Ast
import Eval
import Parser


-- provide tests that show your run/eval works

zero = (LiteralInt 0)
one = (LiteralInt 1)
none = (LiteralInt (-1))
two = (LiteralInt 2)
ntwo = (LiteralInt (-2))
three = (LiteralInt 3)
nthree = (LiteralInt (-3))
four = (LiteralInt 4)
nfour = (LiteralInt (-4))
five = (LiteralInt 5)

-- | Apply a unsafe function
appFunc :: Unsafe Val -> Unsafe Val -> Unsafe Val 
appFunc (Error msg) _ = (Error msg)
appFunc _ (Error msg) = (Error msg)
appFunc (Ok (Fun f)) (Ok val) = f val 
appFunc (Ok val) _ = Error "first input is not a function in appFunc"

-- | test if an unsafe is error
isError :: Unsafe a -> Bool 
isError (Error _) = True 
isError _ = False 

-- | return if the two unsafe has the same error
sameError :: Unsafe a -> Unsafe b -> Bool 
sameError (Error msg1) (Error msg2) = msg1 == msg2
sameError _ _ = False

-- | run a program under empty exprssion
runEmpty :: Ast -> Unsafe Val 
runEmpty exp = runEnvUnsafe (eval exp) Map.empty

runEmptyT :: Ast -> Unsafe TestVal 
runEmptyT exp = 
  case runEnvUnsafe (eval exp) Map.empty of 
    Ok res -> Ok $ valToTestVal res 
    Error msg -> Error msg

tests = testGroup "EvalTest" 
  [
  testCase "type mismatch example" $
      do 
        assertBool "1 + True" $ isError (runEmptyT $ ValInt 1 `Plus` ValBool True)
        assertBool "[] - 2" $ isError (runEmptyT $ Nil `Min` ValInt 2)
        assertBool "1 and True" $ isError (runEmptyT $ ValInt 1 `And` ValBool True)
        assertBool "[] or False" $ isError (runEmptyT $ Nil `Or` ValBool False)
        assertBool "not []" $ isError (runEmptyT $ Not Nil)
        assertBool "Cons [] 3" $ isError (runEmptyT $ Cons Nil (ValInt 3))
        assertBool "if [] then 2 else 3" $ isError (runEmptyT $ If Nil (ValInt 2) (ValInt 3))
        assertBool "App 1 2" $ isError (runEmptyT $ (ValInt 1) `App` (ValInt 2))
        assertBool "2.0 + True" $ isError (runEmptyT $ ValFloat 2.0 `Plus` ValBool True)
        assertBool "2.0 + 1" $ isError (runEmptyT $ ValFloat 2.0 `Plus` ValInt 1)
        assertBool "if [] then 2.0 else 3.0" $ isError (runEmptyT $ If Nil (ValFloat 2) (ValFloat 3))
        assertBool "[] - 2.0" $ isError (runEmptyT $ Nil `Min` ValFloat 2.0)
        assertBool "1.0 and True" $ isError (runEmptyT $ ValFloat 1.0 `And` ValBool True)
        assertBool "Cons [] 3.0" $ isError (runEmptyT $ Cons Nil (ValFloat 3.0))
        assertBool "App 1.0 2.0" $ isError (runEmptyT $ (ValFloat 1.0) `App` (ValFloat 2.0))
        assertBool "1 + Hi" $isError (runEmptyT $ ValInt 1 `Plus` ValString "Hi")
        assertBool "[] - Hi" $isError (runEmptyT $ Nil `Minus` ValString "Hi")
        assertBool "1 and Hi" $ isError (runEmptyT $ ValInt 1 `And` ValString "Hi")
        assertBool "[] or Hi" $ isError (runEmptyT $ Nil `Or` ValString "Hi")
        assertBool "not Hi" $ isError (runEmptyT $ Not ValString "Hi")
        assertBool "Cons [] Hi" $ isError (runEmptyT $ Cons Nil (ValString "Hi"))
        assertBool "if [] then 2 else Hi" $ isError (runEmptyT $ If Nil (ValInt 2) (ValString "Hi"))
        assertBool "App 1 Hi" $ isError (runEmptyT $ (ValInt 1) `App` (ValString "Hi"))
        assertBool "2.0 + Hi" $ isError (runEmptyT $ ValFloat 2.0 `Plus` ValString "Hi")
        assertBool "App 1.0 Hi" $ isError (runEmptyT $ (ValFloat 1.0) `App` (ValString "Hi"))
        assertBool "1.0 and Hi" $ isError (runEmptyT $ ValFloat 1.0 `And` ValString "Hi"),



    testCase "Basic Integer Arithmetic" $
      do 
        assertEqual "2 + 4 =? "    6    (exec (Plus two four))
        assertEqual "2 + -1 =? "   1    (exec (Plus two none))
        assertEqual "2 - 4 =? "    (-2) (exec (Sub two four))
        assertEqual "2 - (-4) =? " 6    (exec (Sub two nfour))
        assertEqual "3 * 2 =? "    6    (exec (Mult three two))
        assertEqual "2 * -2 =? "   (-4) (exec (Mult two ntwo))
        assertEqual "4 // 2 =?"    2    (exec (Div four two))
        assertEqual "4 // -2 =?"   -2   (exec (Div four ntwo)),
        assertEqual "5 // 2 =?"    2    (exec (Div five two))
        assertEqual "5 // 0 =?"  isError (runEmptyT $ ValInt 5 `Div` ValInt 0)
        assertEqual "2 ** 3 =?"    8    (exec (Exp two three))
        assertEqual "2 ** -2 =?"   0    (exec (Exp two ntwo))
        assertEqual "4 mod 2 =?"   0    (exec (Mod four two))
        assertEqual "4 mod 3 =?"   1    (exec (Mod four three))
        assertEqual "-4 =?"        -4   (exec (Neg 4))
        assertEqual "-(-3) =?"     3    (exec (Neg -3)),

    testCase "Compound Arithmetic" $
      do 
        assertEqual "2 + 4 * 3 =? "             14   (exec (Plus two (Mult four three)))
        assertEqual "(2 + -4) * 3 =? "          (-6) (exec (Mult (Plus two nfour) three))
        assertEqual "2 * 3 + 3 * 2 - 4 =? "     8    (exec (Sub (Plus (Mult two three) (Mult three two)) four))
        assertEqual "2 * (3 + 3) * (2 - 4) =? " (-24) (exec (Mult (Mult two (Plus three three)) (Sub two four)))
        assertEqual "2.0 + 4.0 * 3.0 =? "             (ValFloat 14.0)   (Plus (ValFloat 2.0) (Mult (ValFloat 4.0) (ValFloat 3.0)))
        assertEqual "(2.0 + -4.0) * 3.0 =? "          (ValFloat -6.0) (Mult (Plus (ValFloat 2.0) (ValFloat -4.0)) (ValFloat 3.0))
        assertEqual "2.0 * 3.0 + 3.0 * 2.0 - 4.0 =? "     (ValFloat 8.0)    (Sub (Plus (Mult (ValFloat 2.0) (ValFloat 3.0)) (Mult (ValFloat 3.0) (ValFloat 2.0))) (ValFloat 4.0))
        assertEqual "2.0 * (3.0 + 3.0) * (2.0 - 4.0) =? " (ValFloat -24) (Mult (Mult (ValFloat 2.0) (Plus (ValFloat 3.0) (ValFloat 3.0))) (Sub (ValFloat 2.0) (ValFloat 4.0)))),

    testCase "If Statements" $
      do 
        assertEqual "if 3 then 4 else 2 =? "       4  (exec (If three four two))
        assertEqual "if 0 then 1 else 4"           4  (exec (If zero one four))
        assertEqual "if 3 * 0 then 1 else 2  =? "  2  (exec (If (Mult three zero) one two))
        assertEqual "if 3 * 2 then 1 else 2  =? "  1  (exec (If (Mult three two) one two))
        assertEqual "if 3.0 then 4 else 2 =? ",    (ValFloat 4.0) (If (ValFloat 3.0) (ValFloat 4.0) (ValFloat 2.0))
        assertEqual "if 0.0 then 1.0 else 4.0"     (ValFloat 4.0)  (If (ValFloat 0.0) (ValFloat 1.0) (ValFloat 4.0))
        assertEqual "if 3.0 * 0.0 then 1.0 else 2.0  =? "  (ValFloat 2.0)  (If (Mult (ValFloat 3.0) (ValFloat 0.0)) (ValFloat 1.0) (ValFloat 2.0))
        assertEqual "if 3.0 * 2.0 then 1.0 else 2.0  =? "  (ValFloat 1.0)  (If (Mult (ValFloat 3.0)(ValFloat 2.0)) (ValFloat 1.0) (ValFloat 2.0)),


    testCase "Floating Arithmetic" $
      do
        assertEqual "2.0 + 4.0 =? "    6.0    (Plus 2.0 4.0)
        assertEqual "2.0 + -1.0 =? "   1.0    (Plus 2.0 (-1.0))
        assertEqual "1.4 + 2.6 =?"     4.0    (Plus 1.4 2.6)
        assertEqual "2.0 - 4.0 =? "    (-2.0) (Sub 2.0 4.0)
        assertEqual "2.0 - (-4.0) =? " 6.0    (Sub 2.0 (-4.0))
        assertEqual "4.0 - 1.3 =?"     2.7    (Sub 4.0 1.3)
        assertEqual "3.0 * 2.0 =? "    6.0    (Mult 3.0 2.0)
        assertEqual "2.0 * -2.0 =? "   (-4.0) (Mult 2.0 (-2.0))
        assertEqual "1.5 * 3.0 =?"     (4.5)  (Mult 1.5 3.0)
        assertEqual "5.0 / 2.0 =?"     (2.5)  (Div 5.0 2.0)
        assertBool "5.0 / 0.0 =?"      isError (runEmptyT $ ValFloat 5.0 `Div` ValFloat 0.0)
        assertEqual "5.0 / 0.0 =?"     isError (runEmptyT $ ValFloat 5.0 `Div` ValFloat 0.0)
        assertEqual "5.0 / 2.0 =?"     (2.5)  (DivFloat 5.0 2.0)
        assertBool "5.0 / 0.0 =?"     isError (runEmptyT $ ValFloat 5.0 `Div` ValFloat 0.0)
        assertEqual "2.0 ** 3.0 =?"    8.0    (ExpFloat 2.0 3.0)
        assertEqual "2.0 ** -2.0 =?"   0.25   (ExpFloat 2.0 -2.0)
        assertEqual "4.0 mod 2.0 =?"   0.0    (Mod 4.0 2.0)
        assertEqual "4.0 mod 3.0 =?"   1.0    (Mod 4.0 3.0)
        assertEqual "-4.0 =?"          -4.0   (exec (Neg 4))
        assertEqual "-(-3.0) =?"       3.0    (exec (Neg -3))
        assertEqual "-4.0 =?"          -4.0   (Neg 4))
        assertEqual "-(-3.0) =?"       3.0    (Neg -3)),

    testCase "List Function Tests" $
      do 
      	assertEqual "LenList [] =? "  (ValInt 0) (LenList Nil)
      --	xs <- [2,3]
      	assertEqual "LenList [1,2,3] =?" (ValInt 3) (LenList (Cons (ValInt 1) [(ValInt 2),(ValInt 3)]))
      	assertEqual "LenList [1,2,3,3,2] =?" 5 (LenList (Cons (ValInt 1) [(ValInt 2),(ValInt 3),(ValInt 3),(ValInt 2)]))
      	assertEqual "[1,2,3] !! 2 =? " (ValInt 3) (IndexOp (Cons (ValInt 1) [(ValInt 2),(ValInt 3)]) (ValInt 2)
      	assertEqual "[1,2,37,8,9,20] !! 4 =? " (ValInt 9) (IndexOp (Cons (ValInt 1) [(ValInt 2),(ValInt 37),(ValInt 8),(ValInt 9),(ValInt 20)]) ValInt 4)
      	assertBool "[] !! 10 =?"  isError (runEmptyT $ Nil (!!) ValInt 10)
      	assertEqual "LenList [1,2,3] =?" (ValInt 3) (LenList (Cons (ValInt 1) [(ValInt 2),(ValInt 3)]))
      	assertEqual "LenList [1,2,3,3,2] =?" (ValInt 5) (LenList (Cons (ValInt 1) [(ValInt 2),(ValInt 3),(ValInt 3),(ValInt 2)]))
      	assertEqual "ListConcat [1,2,3] [4,5,6] =?" (Cons 1 [2,3,4,5,6]) (ListConcat (Cons (ValInt 1) [(ValInt 2),(ValInt 3)]) (Cons (ValInt 4) [(ValInt 5),(ValInt 6)]))
      	assertEqual "ListConcat [1] [7,8,9] =?" (Cons 1 [7,8,9]) (ListConcat (Cons (ValInt 1) Nil) (Cons (ValInt 7) [(ValInt 8),(ValInt 9)]))
      	assertEqual "ListConcat [Hello] [There] =?" (Cons (ValString Hello) [ValString There]) (ListConcat (Cons (ValString Hello) Nil) (Cons (ValString There) Nil)),
    testCase "Seperator Test" $
      do 
      	assertEqual "3;2 =?" (ValInt 2) (Seperator (ValInt 3) (ValInt 2))
      	assertEqual "True && False || True; 7 =?" (ValInt 7) (Seperator (((ValBool True) `And` (ValBool False)) `Or` (ValBool True)) (ValInt 7))
        assertEqual "39; True =?" (ValBool True) (Seperator (ValInt 39) (ValBool True))
        assertEqual "Hi; 17 =?" (ValInt 17) (Seperator (ValString Hi) ((ValInt 17))),

    testCase = "Boolean Operators" $
      do 
      	assertEqual "True && True" (ValBool True) ((ValBool True) `And` (ValBool True))
      	assertEqual "True && False" (ValBool False) ((ValBool True) `And` (ValBool False))
      	assertEqual "False && False" (ValBool False) ((ValBool False) `And` (ValBool False))
      	assertEqual "True || True" (ValBool True) ((ValBool True) `Or` (ValBool True))
      	assertEqual "True || False" (ValBool True) ((ValBool True) `Or` (ValBool False))
      	assertEqual "False || False" (ValBool False) ((ValBool False) `Or` (ValBool False))
      	assertEqual "Not True" (ValBool False) (Not (ValBool True))
      	assertEqual "Not False" (ValBool True) (Not (ValBool True))
      	assertEqual "True && (Not True) || True" (ValBool True) (((ValBool True) `And` ((Not (ValBool True)))) `Or` (ValBool True))

    testCase "Relational Operators" $
      do
        assertBool "3 is not less than 2!" (LessThan (ValInt 3) (ValInt 2))
        assertBool "-4 is not less than -5!" (LessThan (ValInt (-4)) (ValInt (-5)))
        assertBool "1.2 is not less than 1.1!" (LessThan (ValFloat 1.2) (ValFloat 1.1))
        assertBool "True is not less than False!" (LessThan (ValBool True) (ValBool False))
        assertBool "4 is not equal to 5!" (Eq (ValInt 4) (ValInt 5))
        assertBool "4 is not equal to 4.0!" (Eq (ValInt 4) (ValFloat 4.0))
        assertBool "True is not equal to False!" (Eq (ValBool True) (ValBool False))
        assertBool "6 is equal to -(-6)!" (Neq (ValInt 6) (ValInt (Neg (-6))))
        assertBool "2 is not greater than 3!" (GreaterThan (ValInt 2) (ValInt 3))
        assertBool "-5 is not greater than -4!" (GreaterThan (ValInt (-5)) (ValInt (-4)))
        assertBool "-4 is not less than or equal to -5!" (LtOrEqto (ValInt (-4)) (ValInt (-5)))
        assertBool "-5 is not greater than or equal to -4!" (GtOrEqto (ValInt (-5)) (ValInt (-4))),

    testProperty "nested function: \\x -> \\y -> x + y" $
      \x y ->
        let 
          res =  runEmpty $ Lam "x" $ Lam "y" $ (Var "x") `Plus` (Var "y")
        in 
          (res `appFunc` Ok (I x) `appFunc` Ok (I y)) `eqInt` (Ok $ I $ x + y),

    testProperty "function as input: \\f -> \\x -> f x" $ 
      \(QC.Fun _ f :: QC.Fun Integer Integer) n -> 
        let 
          res =  runEmpty $ Lam "f"  $ Lam "x" $ (Var "f") `App` (Var "x")
          fVal inp = 
            case inp of 
              I n -> Ok $ I $ f n 
              _ -> Error "f only handles Integer"
        in 
          (res `appFunc` Ok (Fun fVal) `appFunc` Ok (I n)) `eqInt` (Ok $ I $ f n)
        
  ]

