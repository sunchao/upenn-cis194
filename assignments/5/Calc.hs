{-# LANGUAGE FlexibleInstances #-}

module Calc where
import ExprT
import Parser
import qualified StackVM as SVM


calc :: ExprT -> Integer
calc (Lit n) = n
calc (Add e1 e2) = (calc e1) + (calc e2)
calc (Mul e1 e2) = (calc e1) * (calc e2)

-- Evaluates arithmetic expressions given as a String, producing
-- Nothing for inputs which are not well-formed expressions, and Just n
-- for well-formed inputs that evalutes to n
evalStr :: String -> Maybe Integer
evalStr expr = case parseExp Lit Add Mul expr of
                 Nothing -> Nothing
                 Just e -> Just $ calc e


class Expr a where
    add :: a -> a -> a
    mul :: a -> a -> a
    lit :: Integer -> a


instance Expr ExprT where
    add = Add
    mul = Mul
    lit = Lit

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    add = (+)
    mul = (*)
    lit = id

instance Expr Bool where
    add = (||)
    mul = (&&)
    lit = (> 0)


newtype MinMax = MinMax Integer deriving (Show, Eq)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)

instance Expr MinMax where
    add (MinMax a) (MinMax b) = MinMax $ max a b
    mul (MinMax a) (MinMax b) = MinMax $ min a b
    lit = MinMax

instance Expr Mod7 where
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7
    lit = Mod7


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMinMax = testExp :: Maybe MinMax
testMod7 = testExp :: Maybe Mod7


-- Exercise 5

instance Expr SVM.Program where
    add a b = a ++ b ++ [SVM.Add]
    mul a b = a ++ b ++ [SVM.Mul]
    lit i = [SVM.PushI i]

compile :: String -> Maybe SVM.Program
compile expr = parseExp lit add mul expr


