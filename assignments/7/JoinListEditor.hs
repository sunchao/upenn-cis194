module Main where

import Sized
import Scrabble
import JoinList
import Editor

main = runEditor editor $ Single (Score 1, Size 1) "a"
