module Main where


import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.List
import Data.Ord
import Data.Time.Clock

import Huffman


main :: IO ()
main = do
  start <- getCurrentTime

  contents1 <- readFile "resources/data_1.txt"
  contents2 <- readFile "resources/data_2.txt"
  contents3 <- readFile "resources/data_3.txt"
  let allLines = lines contents1 ++ lines contents2 ++ lines contents3

  let frequencies = histogram (concat allLines)
  let sortedFrequencies = sortBy (comparing swap) frequencies
  let huffmanTree = sortedHuffman sortedFrequencies

  let encoding = codes huffmanTree
  let encodedData = map (encode encoding) allLines
  let encodedBits = padToEight (concat encodedData)
  let bits = bitpack encodedBits
  Data.ByteString.Lazy.writeFile "single_thread_compressed_data.bin" bits
  let decoded = map (decode huffmanTree) encodedData
  print (forceList decoded)

  end <- getCurrentTime

  putStrLn "Single thread"
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  

  start <- getCurrentTime

  contents1 <- readFile "resources2/data_1.txt"
  contents2 <- readFile "resources2/data_2.txt"
  contents3 <- readFile "resources2/data_3.txt"
  let allLines = lines contents1 ++ lines contents2 ++ lines contents3

  let frequencies = histogram (concat allLines)
  let sortedFrequencies = sortBy (comparing swap) frequencies
  let huffmanTree = sortedHuffman sortedFrequencies
  
  let encoding = codes huffmanTree
  let encodedData = parallelMap allLines (encode encoding) 0
  let encodedBits = padToEight (concat encodedData)
  let bits = bitpack encodedBits
  Data.ByteString.Lazy.writeFile "multiple_thread_compressed_data.bin" bits
  let decoded = parallelMap encodedData (decode huffmanTree) 0
  print (forceList decoded)

  end <- getCurrentTime

  putStrLn "Multiple thread"
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
