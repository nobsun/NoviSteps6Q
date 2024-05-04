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

-- import Debug.Trace

type I = String
type O = Int

type Solver = (Int,Int) -> O

solve :: Solver
solve = \ case
    (p,q) -> tbl
           & drop p
           & head
           & drop (pred q - p)
           & head
        where
            tbl = scanl1 (+) <$> tails ps

ps :: [Int]
ps = [3,1,4,1,5,9]

wrap :: Solver -> ([[I]] -> [[O]])
wrap f = \ case
    [p,q]:_ -> case sort (subtract (ord 'A') . ord . head <$> [p,q]) of
        [r,s] -> [[f (r,s)]]
        _   -> error "wrap: invalid input format"
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
