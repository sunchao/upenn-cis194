{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
ja +++ jb = Append ((tag ja) `mappend` (tag jb)) ja jb


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ n (Append m ja jb)
    | n < k = indexJ n ja
    | otherwise = indexJ (n-k) jb
    where k = getSize . size . tag $ ja

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n j | n <= 0 = j
dropJ n (Single _ _) = Empty
dropJ n (Append m ja jb)
    | n >= (getSize . size $ m) = Empty
    | n >= k = dropJ (n - k) jb
    | otherwise = (dropJ n ja) +++ jb
    where k = getSize . size . tag $ ja

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ n s@(Single _ _) = s
takeJ n a@(Append m ja jb)
    | n >= (getSize . size $ m) = a
    | n >= k = ja +++ (takeJ (n-k) jb)
    | otherwise = takeJ k ja
    where k = getSize . size . tag $ ja



scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ s) = s
    toString (Append m sa sb) = toString sa ++ toString sb

    fromString s = Single ((scoreString s), Size 1) s

    line = indexJ

    replaceLine n s j = takeJ (n-1) j +++ fromString s +++ dropJ n j

    numLines Empty = 0
    numLines (Single _ _) = 1
    numLines (Append (_,s) _ _) = getSize s

    value Empty = 0
    value (Single (v,_) _) = getScore v
    value (Append (v,_) _ _) = getScore v
