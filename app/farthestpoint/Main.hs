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

import Data.Array
import Data.Bool
import Data.Char
import Data.Ord
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Vector qualified as V

import Debug.Trace

type I = Int
type O = Int

type Solver = (I,[[I]]) -> [I]

solve :: Solver
solve = \ case
    (n,ps) -> case listArray (1,n) ps of
        tbl -> case [(p,q) | p <- [1..n], q <- [1..n], p /= q] of
            cs -> case slice (pred n) cs of
                css -> snd . mmax <$> css
                    where
                        mmax = foldl1 phi
                        phi (i,j) (_,k) = case dist i j `compare` dist i k of
                            LT -> (i,k)
                            _  -> (i,j)
                        dist i j = sum $ zipWith (((^(2::Int)) .) . subtract) (tbl ! i) (tbl ! j)

slice :: Int -> [a] -> [[a]]
slice n xs = case splitAt n xs of
    (ys,[]) -> [ys]
    (ys,zs) -> ys : slice n zs

wrap :: Solver -> ([[I]] -> [[O]])
wrap f = \ case
    [n]:ps -> case f (n,ps) of
        r -> (:[]) <$> r
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
