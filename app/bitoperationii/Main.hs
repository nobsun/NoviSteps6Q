module Main where

import Data.ByteString.Char8 qualified as B
import Data.Maybe
import Data.Ord
import Data.Bits

import Data.List
import Text.Printf

type I = Int
type O = String

type Solver = (I,I) -> [O]

solve :: Solver
solve (a,b) = map (printf "%032b") [a .&. b, a .|. b, a `xor` b]

wrap :: Solver -> ([[I]] -> [[O]])
wrap f dds = case dds of
    [a,b]:_ -> case f (a,b) of
        rr -> singleton <$> rr
    _   -> error $ "wrap: invalid input format" ++ "?" ++ show dds

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

{- Error -}
impossible :: a
impossible = error "impossible"

invalid :: a
invalid = error "invalid input"

