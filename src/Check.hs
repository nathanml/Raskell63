module Check where

import Data.Set (Set)
import qualified Data.Set as Set
import HelpShow
import Ast


-- here you can preform static checks


-- | The data type for all the static check warning
-- Some example include:
--   * use of undefined variable
--   * defined but unused variable
--   * type errors
data WarningMsg = 
    UndefinedVarUse String  -- ^ This is the Warning for use of Undefined variable name
  -- ...
  deriving (Show,Eq)


typeOf :: Ast -> Ast
typeOf ast = undefined



-- | perform static checking on the Ast
-- the output a set of warning on that input Ast
-- resembles freeVars
check :: Ast -> Set WarningMsg
check (Var x) = (Set.insert x) (UndefinedVarUse String)
check (App t1 t2) = ((check t1) `Set.union` (check t2)) (UndefinedVarUse String)
check (Lam x l) = Set.delete x (check l) (UndefinedVarUse String)

