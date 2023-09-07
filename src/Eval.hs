module Eval where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude
import Data.Char
import Ast
import EnvUnsafe


-- the goal of the program is to return a value, what values are possible?
data Val = I Int | B Bool | S String | F Float | C Char
         | Ls [Val]
         | Fun (Val -> Unsafe Val)
           -- since this is a functional language, one thing that can be returned is a function

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (Ls ls) = show ls
  show (F f) = show f
  show (Fun _) = "\\ x -> ?" -- no good way to show a function

stdLib = Map.fromList

  [("tail", Fun $ \ v -> case v of Ls (_:ls) -> Ok $ Ls ls
                                   _         -> Error "can only call tail on a non empty list"),
   ("head", Fun $ \ v -> case v of Ls []         -> Error "Empty list"
                                   Ls ((I h):_)  -> Ok (I h)
                                   Ls ((B h):_)  -> Ok (B h)
                                   I h           -> Ok (I h)
                                   B h           -> Ok (B h)
                                   _             -> Error "can only call head on a non empty list"),
  -- ("elem", Fun $ \v -> case v of Fun f -> ((Error "functions cannot be inputs"))
    --                              _     -> (Ok (Fun $ \ls -> case ls of
      --                                                       Ls ls -> (Ok (B (elem v ls))) 
        --                                                     _ -> ((Error "input must be a list"))))),
  

   ("ord", Fun $ \v -> case v of C c -> (Ok (I(digitToInt c)))
                                 _   -> ((Error "Must input a char"))),
   ("chr", Fun $ \v -> case v of I i -> (Ok (C (intToDigit i)))
                                 _   -> ((Error "you must input an int"))),

  -- ("float", Fun $ \v -> case v of I f -> ((Ok (F (fromInteger f))))
                             --       _  -> ((Error "you must input an int"))),
   ("int", Fun $ \v -> case v of F f -> ((Ok (I (truncate f))))
                                 _ -> (Error "you must input a Float"))
   ]

                                 
type Env = Map String Val

{--
mapHelper :: Fun
mapHelper _ [] = (Ok (Ls []), []) 
mapHelper f (x:[]) = case (f x) of ((Ok a), s) -> ((Ok (Ls [a]), s))
mapHelper f (x: xs) = case (f x), (mapHelper f xs) of ((Ok (Ls a)), s), ((Ok (Ls b)), v) -> ((Ok (Ls (a ++ b))), (s ++ v))
--}
evalInt :: Ast -> EnvUnsafe Env Int
evalInt a = do a' <- eval a
               case a' of
                I i -> return i
                _ -> err "Not the int we expected!"

evalFloat :: Ast -> EnvUnsafe Env Float
evalFloat a = do a' <- eval a
                 case a' of 
                   F f -> return f
                   _ -> err "Not the float we expect"

evalBool :: Ast -> EnvUnsafe Env Bool
evalBool x = do x' <- eval x
                case x' of
                 B b -> return b
                 _   -> err "Not the bool we expect"

evalFun :: Ast -> EnvUnsafe Env (Val-> Unsafe Val)
evalFun x = do result <- eval x
               case result of 
                Fun function -> return function 
                _ -> err "not the function we expect"

evalList :: Ast -> EnvUnsafe Env [Val]
evalList (Cons x xs) = do ls <- eval (Cons x xs)
                          case ls of
                           Ls [lst] -> return [lst]
                           _ -> err "not the list we expect"
                        



eval :: Ast -> EnvUnsafe Env Val
eval (Plus l r) = do l' <- evalInt l
                     r' <- evalInt r
                     return $I$ l' +  r'

eval (PlusFloat l r) = do l' <- evalFloat l
                          r' <- evalFloat r
                          return $F$ l' + r'

eval (Min l r) = do l' <- evalInt l
                    r' <- evalInt r
                    return $ I $ l' - r'

eval (MinFloat l r) = do l' <- evalFloat l
                         r' <- evalFloat r
                         return $ F $ l' - r'

eval (Mult l r) = do l' <- evalInt l
                     r' <- evalInt r
                     return $ I $ l' * r'

eval (MultFloat l r) = do l' <- evalFloat l
                          r' <- evalFloat r
                          return $ F $ l' * r'

eval (Div l r) = do l' <- evalInt l
                    r' <- evalInt r
                    case r' of 
                     (0) -> err "cannot divide by 0"
                     (_) -> return $I$ (l' `div` r')

eval (DivFloat l r) = do l' <- evalFloat l
                         r' <- evalFloat r
                         case r' of 
                          (0) -> err "cannot divide by 0"
                          (_) -> return $F$ ((/) l' r')

eval (ExpFloat l r) = do r' <- evalFloat r
                         l' <- evalFloat l
                         return $ F $ ( l' ** r')

eval (Exp l r) = do r' <- evalInt r
                    l' <- evalInt l
                    return $I$ (l'^r')

eval (Mod l r) = do l' <- evalInt l
                    r' <- evalInt r
                    case r' of
                      (0) -> err "cannot mod by 0"
                      (_) -> return $I$ (l' `mod` r')

eval (Neg x) = do x' <- evalInt x
                  case x' of
                   i -> return $I$ (0-i)

eval (NegFloat x) = do x' <- evalFloat x
                       case x' of 
                        f -> return $F$ (0-f)


eval (Separator l r) = do r' <- eval r
                          return r'

eval (App function argument) = do func <- evalFun function
                                  argu <- eval argument
                                  case func argu of
                                    Error s -> err s
                                    Ok finalResult -> return finalResult

eval (And x y) = do x' <- evalBool x
                    y' <- evalBool y
                    case x' of
                      False -> return $B$ False
                      _ -> case y' of
                        False -> return $B$ False
                        _ -> return $B$ True          

eval (Or x y) = do x' <- evalBool x
                   y' <- evalBool y
                   case x' of
                   	    True -> return $B$ True
                   	    _ -> case y' of
                   	    	True -> return $B$ True
                   	    	_ -> return $B$ False

eval (Not r) = do result <- evalBool r
                  return $ B (not result)

eval (Eq x y) = do x' <- eval x
                   y' <- eval y
                   case x' of
                   	    y' -> return $B$ True
                   	    _ -> return $B$ False

eval (Neq x y) = do x' <- eval x
                    y' <- eval y
                    case x' of
                   	  y' -> return $B$ False
                   	  _ -> return $B$ True

eval (LessThan x y) = do x' <- evalInt x
                         y' <- evalInt  y
                         z' <- eval(ValBool (x' < y'))
                         return  z'

eval (LtorEqto x y) = do x' <- evalInt x
                         y' <- evalInt y
                         z' <- eval(ValBool (x' <= y'))
                         return  z'


eval (GreaterThan x y) = do x' <- evalInt  x
                            y' <- evalInt  y
                            z' <- eval(ValBool (x' > y'))
                            return  z'
                            
eval (GtorEqto x y) = do x' <- evalInt x
                         y' <- evalInt y
                         z' <- eval(ValBool (x' >= y'))
                         return  z'
eval (LessThanF x y) = do x' <- evalFloat x
                          y' <- evalFloat  y
                          z' <- eval(ValBool (x' < y'))
                          return  z'

eval (LtorEqtoF x y) = do x' <- evalFloat x
                          y' <- evalFloat y
                          z' <- eval(ValBool (x' <= y'))
                          return  z'


eval (GreaterThanF x y) = do x' <- evalFloat  x
                             y' <- evalFloat  y
                             z' <- eval(ValBool (x' > y'))
                             return  z'
                            
eval (GtorEqtoF x y) = do x' <- evalFloat x
                          y' <- evalFloat y
                          z' <- eval(ValBool (x' >= y'))
                          return  z'

eval (Nil) = return $ Ls []

eval (Cons x y) = do x' <- eval x
                     case x' of
                      Ls [] -> err "Can't creat list with first value as empty list"
                      _     -> do y' <- eval y
                                  case y' of
                                   Ls ys -> return $ Ls $ [x'] ++ ys
                                   B b   -> return $ Ls $ [x'] ++ [B b]
                                   I i   -> return $ Ls $ [x'] ++ [I i]
                                   Fun f -> return $ Ls $ [x'] ++ [Fun f]
                                   _     -> err "Second argument is not a list"


eval (IndexOp lst val) = do val' <- evalInt val
                            lst' <- evalList lst
                            case lst' of
                              [B b] -> return ((!!) lst' val')
                              [I i] -> return ((!!) lst' val')
                              [S s] -> return ((!!) lst' val')
                              [C c] -> return ((!!) lst' val')
                              [F f] -> return ((!!) lst' val') 


     

-- | helper function that runs with the default environment (for example, the stdLib in week 10)
-- return either the error string or the value, along with everything that was printed
run :: Ast  -- ^ run this Ast
      -> (Either String Val, [String])  -- ^ (error message or result value, all the printings)
run a = undefined 

