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
    ds -> zipWith (curry count) [1..] ds & sum

count :: (Int,Int) -> Int
count = \ case
    (m,d) | m < 10    -> length [ () | x <- [1 .. d], zorome (intToDigit m) x ]
          | otherwise -> case elemIndex m zs of
            Nothing   -> 0
            Just n    -> length [ () | x <- [1 .. d], zorome (intToDigit n) x ]
    where
        zorome c x = all (c ==) (show x)
        zs = [00,11,22,33,44,55,66,77,88,99]

wrap :: Solver -> ([[I]] -> [[O]])
wrap f = \ case
    _:ds:_ -> case f ds of
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
