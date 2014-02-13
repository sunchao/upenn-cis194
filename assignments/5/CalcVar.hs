{-# LANGUAGE FlexibleInstances #-}

module CalcVar where
import Calc
import qualified Data.Map as M
import Control.Monad

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
                deriving (Show, Eq)

instance Expr VarExprT where
    add = Add
    mul = Mul
    lit = Lit

instance HasVars VarExprT where
    var s = Var s

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    add a b m = liftM2 (+) (a m) (b m)
    mul a b m = liftM2 (*) (a m) (b m)
    lit i = const $ Just i

withVars :: [(String,Integer)]
           -> (M.Map String Integer -> Maybe Integer)
           -> Maybe Integer

withVars vs exp = exp $ M.fromList vs