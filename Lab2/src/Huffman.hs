module Huffman where


import Control.Monad
import qualified Data.Binary.BitPut
import qualified Data.Binary.Strict.BitGet
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.Char
import Data.List
import qualified Data.Map
import Data.Maybe
import Data.Ord
import GHC.Conc.Sync
import GHC.Conc


data HuffmanTree a
  = LeafNode a Int
  | InternalNode Int (HuffmanTree a) (HuffmanTree a)
  deriving (Eq)

instance Show a => Show (HuffmanTree a) where
  show = go ""
    where
      spaces = map (const ' ')
      paren s = "(" ++ s ++ ")"
      go ss (LeafNode s o) = "══" ++ paren (show o) ++ show s ++ "\n"
      go ss (InternalNode o l r) =
        let root = "══" ++ paren (show o) ++ "═╦"
            ss' = ss ++ tail (spaces root)
            lbranch = go (ss' ++ "║") l
            rbranch = go (ss' ++ " ") r
         in root ++ lbranch ++ ss' ++ "║\n" ++ ss' ++ "╚" ++ rbranch


frequency :: HuffmanTree a -> Int
frequency (LeafNode _ x)       = x
frequency (InternalNode x _ _) = x


sortedHuffman :: [(a, Int)] -> HuffmanTree a
sortedHuffman
 = combine . map toLeaf
  where
    combine [t] = t
    combine (ta:tb:ts) = combine . insertBy (comparing frequency) (merge ta tb) $ ts
    merge ta tb = InternalNode (frequency ta + frequency tb) ta tb
    toLeaf = uncurry LeafNode

codes :: Ord a => HuffmanTree a -> Data.Map.Map a [Bool]
codes = Data.Map.fromList . go []
  where
    go p (LeafNode s _)       = [(s, reverse p)]
    go p (InternalNode _ l r) = go (False : p) l ++ go (True : p) r

encode :: Ord a => Data.Map.Map a [Bool] -> [a] -> [Bool]
encode tbl = concatMap get
  where
    get x = fromJust (Data.Map.lookup x tbl)

parallelMap :: [a] -> (a -> b) -> Int -> [b]
parallelMap [] _ _ = []
parallelMap (x:xs) func counter 
  | counter <= numCapabilities*2 = resultTail `par` force resultHead `pseq` resultHead : resultTail
  | otherwise = resultHead:resultTail
  where
    resultHead = func x
    resultTail = parallelMap xs func (counter + 1)


force :: a -> ()
force element = element `pseq` ()

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `pseq` forceList xs


decode :: HuffmanTree a -> [Bool] -> [a]
decode t0 xs0 = go t0 xs0
  where
    go (LeafNode s _) bs = s : go t0 bs
    go (InternalNode _ l r) (b:bs)
      | not b = go l bs
      | otherwise = go r bs
    go _ [] = []

histogram :: Ord a => [a] -> [(a, Int)]
histogram = Data.Map.toList . foldl' insert Data.Map.empty
  where
    insert a k = Data.Map.insertWith (+) k 1 a

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

showBits :: [Bool] -> String
showBits = map (intToDigit . fromEnum)

bitpack :: [Bool] -> Data.ByteString.Lazy.ByteString
bitpack = Data.Binary.BitPut.runBitPut . mapM_ Data.Binary.BitPut.putBit

bitunpack :: Data.ByteString.ByteString -> Either String [Bool]
bitunpack bs0 = Data.Binary.Strict.BitGet.runBitGet bs0 $ go []
  where
    go a = do
      e <- Data.Binary.Strict.BitGet.isEmpty
      if e
        then return (reverse a)
        else Data.Binary.Strict.BitGet.getBit >>= go . (: a)

padToEight :: [Bool] -> [Bool]
padToEight bits =
  let len = length bits
      rem = len `mod` 8
      extra = 8 - rem
      padding = replicate extra False
   in bits ++ padding
