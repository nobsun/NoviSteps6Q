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

type I = Int
type O = Int

type Solver = [I] -> O

solve :: Solver
solve = \ case
    xs -> mex . IS.toList . IS.fromList $ xs

wrap :: Solver -> ([[I]] -> [[O]])
wrap f = \ case
    _:xs:_ -> case f xs of
        rr -> [[rr]]
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

