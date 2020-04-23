#!/usr/bin/runhugs 

import System.Environment
import Data.Char

atoi :: [Char] -> Int
atoi cs = sum [int c * 10^(n-i-1) | (c,i) <- zip cs [0..]]    
    where
      int c = ord c - ord '0'
      n = length cs

fact :: Int -> Int
fact 1 = 1
fact n = n * fact (n-1) 

main = do
         (n:_) <- getArgs
         print (fact (atoi (n)))
