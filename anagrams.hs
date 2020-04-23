#!/usr/bin/runhugs 

import System.IO
import Control.Monad
import Debug.Trace

qsort :: Ord t => [t] -> [t]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where 
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b >  x]

bsearch :: Ord a => a -> [a] -> Int
bsearch a [] = -1
bsearch a as = bfind 0 (length as)
               where
                 bfind :: Int -> Int -> Int
                 bfind lhs rhs | lhs > rhs = -1 
                               | as !! mid == a  = mid
                               | as !! mid >  a  = bfind lhs (mid-1)
                               | otherwise = bfind (mid+1) rhs
                               where 
                                 mid = div (lhs+rhs) 2
                                 --_ = traceId ("mid" ++ (show mid))

substr :: Int -> Int -> String -> String
substr i j s = take (j-i) (drop i s)

-- hacky / buggy --
split :: Char -> String -> [String]
split c s = [substr (i+1) j s | (i,j) <- is', j > i+1]
             where
               cs = [i | (c',i) <- zip s [0..], c' == c]
               is' = zip (-1:cs) (cs ++ [length(s)])

pp :: [Char] -> IO ()
pp cc = qq cc False
          where 
            qq :: [Char] -> Bool -> IO ()
            qq        []     _ = return ()
            qq  (' ':ss) False = do putChar '\n'
                                    qq ss True
            qq    (s:ss) False = do putChar s 
                                    qq ss False
            qq ('\n':ss)  True = qq ss False
            qq    (_:ss)  True = qq ss True

tests = do print "running tests"
           assertx "   /aeeegimnnnnoss" (qsort "no sense / meaning")
           assertx "123abc" (qsort "312bac")
           assertx ""       (qsort "")
           asserta [1,2,3]  (qsort [3,2,1])
           --asserta []       (qsort [])

           assertx "cdef" (substr 2 6 "abcdefg")
           assertx ""     (substr 0 0 "abcdefg")
           assertx "a"    (substr 0 1 "abcdefg")
           assertx ""     (substr 1 1 "abcdefg")
           assertx "b"    (substr 1 2 "abcdefg")

           assertx ["a","car","is","a","friend",".."] (split ' ' "   a car   is a friend   .. ")

           assertx (-1) (-1)
           asserta [1]  [1]

           assertx 2    (bsearch 'i' "aeiou")
           assertx (-1) (bsearch 'x' "aeiou")
           assertx 0    (bsearch 'a' "aeiou")
           assertx 4    (bsearch 'u' "aeiou")
           assertx 1    (bsearch 'u' "ou")
           assertx 0    (bsearch 'u' "u")
           assertx (-1) (bsearch 'u' "")
           assertx 2    (bsearch 'i' "aeiouy")
           assertx (-1) (bsearch 'x' "aeiouy")
           assertx 0    (bsearch 'a' "aeiouy")
           assertx 4    (bsearch 'u' "aeiouy")
 
           assertx "abc" (wordOne "abc 1 2")
           assertx "a"   (wordOne "a")
           assertx ""    (wordOne "")

           asserta [1,2] (uniq [1,1,2,2])
           asserta [1]   (uniq [1,1,1])
           print "done."

uniq :: Eq t => [t] -> [t]
uniq [] = []
uniq (a1:a2:as) | a1 == a2    = uniq (a2:as)
                | otherwise   = a1:uniq (a2:as)
uniq (a1:a2)    | [a1] == a2  = a2
                | otherwise   = a1:a2

wordOne :: [Char] -> [Char]
wordOne [] = []
wordOne (' ':_) = []
wordOne (s:ss) =  s : wordOne ss

assertx :: (Eq t, Show t) => t -> t -> IO ()
assertx a b | a == b    = return () 
            | otherwise = error (show(a) ++ " /= " ++ show(b))

asserta :: (Eq t, Show t) => [t] -> [t] -> IO ()
asserta a b | a == b    = return () 
            | otherwise = error (show(a) ++ " /= " ++ show(b))

puts :: [String] -> IO ()
puts []     = return ()
puts [a]    = putStrLn a
puts (a:as) = do putStrLn a 
                 puts as

findEm :: (Ord a, Show a) => [a] -> [a] -> IO ()
findEm [] _ = return ()
findEm (s:ss) pool = do 
                       putStrLn (show idx ++ ":" ++ show s)
                       findEm ss pool
                     where
                       idx = bsearch s pool

main = do
         tests
         contents <- readFile "words"
         let words = map wordOne (lines contents)
         print ("read " ++ show(length words) ++ " words")

         let wordss = qsort words
         print ("sorted " ++ show(length wordss) ++ " words")

         let wordsu = uniq words
         print ("uniq'd " ++ show(length wordsu) ++ " words")

         findEm wordsu wordsu

         --puts words
   
