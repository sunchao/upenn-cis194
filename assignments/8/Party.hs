module Party where

import Employee
import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)


instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL xs f1) (GL ys f2) = GL (xs ++ ys) (f1+f2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f base Node { rootLabel = r, subForest = [] } = f r base
treeFold f base Node { rootLabel = r, subForest = s }
    = f r (foldl (treeFold f) base s)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp xs = (glCons emp . mconcat . fst $ unzip xs,
                    mconcat $ map (uncurry moreFun) xs)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . maxFun'

maxFun' :: Tree Employee -> (GuestList, GuestList)
maxFun' (Node e fo) = nextLevel e $ map maxFun' fo


formatList :: GuestList -> [String]
formatList (GL es fun) = ("Total fun: " ++ show fun) : map empName es


main :: IO ()
main = do
  file <- readFile "company.txt"
  mapM_ putStrLn . formatList . maxFun $ read file
