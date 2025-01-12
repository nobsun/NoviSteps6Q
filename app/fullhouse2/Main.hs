{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main where

import Data.ByteString.Char8 qualified as B
import Data.Maybe
import Data.Ord

import Data.Array
import Data.Bool
import Data.Char
import Data.Function

import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V

import Debug.Trace qualified as Debug

debug :: Bool
debug = () /= ()

type I = Int
type O = String

type Solver = [I] -> O

solve :: Solver
solve = \ case
    abcd -> case length $ group $ sort abcd of
        2 -> "Yes"
        _ -> "No"


wrap :: Solver -> ([[I]] -> [[O]])
wrap f = \ case
    abcd@[_,_,_,_]:_ -> case f abcd of
        r -> [[r]]
    _   -> error "wrap: invalid input format"

main :: IO ()
main = B.interact (encode . wrap solve . decode)

class InterfaceForOJS a where
    readB :: B.ByteString -> a
    readBs :: B.ByteString -> [a]
    readBs = map readB . B.words
    decode :: B.ByteString -> [[a]]
    decode = map readBs . B.lines

    showB :: a -> B.ByteString
    showBs :: [a] -> B.ByteString
    showBs = B.unwords . map showB
    encode :: [[a]] -> B.ByteString
    encode = B.unlines . map showBs

instance InterfaceForOJS B.ByteString where
    readB = id
    showB = id

instance InterfaceForOJS Int where
    readB = readInt
    showB = showInt

instance InterfaceForOJS String where
    readB = readStr
    showB = showStr

instance InterfaceForOJS Double where
    readB = readDbl
    showB = showDbl

instance InterfaceForOJS Char where
    readB = B.head
    showB = B.singleton
    readBs = B.unpack
    showBs = B.pack

readInt :: B.ByteString -> Int
readInt = fst . fromJust . B.readInt

showInt :: Int -> B.ByteString
showInt = B.pack . show

readStr :: B.ByteString -> String
readStr = B.unpack

showStr :: String -> B.ByteString
showStr = B.pack

readDbl :: B.ByteString -> Double
readDbl = read . B.unpack

showDbl :: Double -> B.ByteString
showDbl = B.pack . show

{- Trace -}
trace :: String -> a -> a
trace | debug     = Debug.trace
      | otherwise = const id

traceShow :: Show a => a -> b -> b
traceShow | debug     = Debug.traceShow
          | otherwise = const id

{- Error -}
impossible :: a
impossible = error "impossible"

invalid :: a
invalid = error "invalid input"

{- Bonsai -}

{- |
>>> combinations 2 "abcd"
["ab","ac","ad","bc","bd","cd"]
-}
combinations :: Int -> [a] -> [[a]]
combinations = \ case
    0   -> const [[]]
    n+1 -> \ case 
        []   -> []
        x:xs -> map (x:) (combinations n xs) ++ combinations (n+1) xs
    _ -> error "negative"

{- |
>>> spanCount odd [3,1,4,1,5,9]
(2,[4,1,5,9])
-}
spanCount :: (a -> Bool) -> [a] -> (Int, [a])
spanCount p = \ case
    []   -> (0,[])
    aas@(a:as)
        | p a       -> case spanCount p as of
            (c,bs)      -> (succ c, bs)
        | otherwise -> (0,aas)
{- |
>>> runLength "aaaabbbcddeeeeeefghhhh"
[('a',4),('b',3),('c',1),('d',2),('e',6),('f',1),('g',1),('h',4)]
-}
runLength :: Eq a => [a] -> [(a, Int)]
runLength = unfoldr phi
  where
    phi []     = Nothing
    phi (x:xs) = case spanCount (x ==) xs of
      (m, zs) -> Just ((x, succ m) , zs)

{- |
無限リストの無限リストをマージする
-}
merges :: Ord a => [[a]] -> [a]
merges = foldr1 xmerge where
    xmerge = \ case
        !x:xs    -> \ case
            ys        -> x : merge xs ys
        _       -> invalid
    merge = \ case
        xxs@(x:xs) -> \ case
            yys@(y:ys) -> case compare x y of
                LT -> x : merge xs yys
                EQ -> x : merge xs ys
                GT -> y : merge xxs ys
            _ -> invalid
        _ -> invalid

{- |
>>> splitEvery 3 [0 .. 10]
[[0,1,2],[3,4,5],[6,7,8],[9,10]]
-}
splitEvery :: Int -> [a] -> [[a]]
splitEvery k = \ case
    [] -> []
    xs -> case splitAt k xs of
        (ys,zs) -> ys : splitEvery k zs

{- |
>>> mex [8,23,9,0,12,11,1,10,13,7,41,4,14,21,5,17,3,19,2,6]
15
-}
mex     ::  [Int] -> Int
mex xs  =   minform 0 (length xs, xs)

minform         ::  Int -> (Int, [Int]) -> Int
minform a (n,xs)
  | n == 0      =   a
  | m == b - a  =   minform b (n-m, vs)
  | otherwise   =   minform a (m, us)
    where  (us,vs)  =  partition (< b) xs
           b        =  a + 1 + n `div` 2
           m        = length us
{- misc -}
toTuple :: [a] -> (a,a)
toTuple = \ case
    x:y:_ -> (x,y)
    _     -> invalid

fromTuple :: (a,a) -> [a]
fromTuple (x,y) = [x,y]

countif :: (a -> Bool) -> [a] -> Int
countif = iter 0
    where
        iter a p (x:xs) = iter (bool a (succ a) (p x)) p xs
        iter a _ []     = a

{- |
リストの要素交換
>>> swapElems 2 5 [3,1,4,1,5,9,2,6,5]
[3,1,9,1,5,4,2,6,5]
-}
swapElems :: Int -> Int -> [a] -> [a]
swapElems i j xs = case compare i j of
    GT -> swapElems j i xs
    EQ -> xs
    LT -> case splitAt i xs of
        (_,[])    -> error "swapElems: out of range"
        (ys,z:zs) -> case splitAt (pred (j - i)) zs of
            (_,[])    -> error "swapElems: out of range"
            (us,v:vs) -> ys ++ v : (us ++ z : vs)

{- Sized List -}
type SzL a = ([a], Int)

nil :: SzL a
nil = ([], 0)

isNil :: SzL a -> Bool
isNil = \ case
    ([],_)   -> True
    _        -> False


cons :: a -> SzL a -> SzL a
cons x (xs, n) = (x:xs, succ n)

hd :: SzL a -> a
hd = sizedList (error "hd: empty Sized List") const 

tl :: SzL a -> SzL a
tl = sizedList (error "tl: empty Sized List") (const id)

singleton :: a -> SzL a
singleton x = cons x nil

sizedList :: b -> (a -> SzL a -> b) -> SzL a -> b
sizedList y f xxs = case xxs of
    ([],   _) -> y
    (x:xs, n) -> f x (xs, pred n)

size :: SzL a -> Int
size = snd


