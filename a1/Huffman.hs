module Huffman where

import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           LeftistHeap (PQueue)
import qualified LeftistHeap as PQueue

import           HuffmanDef

decodeHelper :: HuffmanTree -> HuffmanTree -> [Bool] -> [Char]
decodeHelper (Leaf f c) o [] = [c]
decodeHelper (Leaf f c) o xs = c : decodeHelper o o xs
decodeHelper (Branch f l r) o (x:xs) =
  if x then decodeHelper r o xs
  else decodeHelper l o xs

decode :: HuffmanTree -> [Bool] -> [Char]
decode h b = decodeHelper h h b

huffmanTreeBuildQueueHelper :: [(Char, Int)] -> PQueue Int HuffmanTree
huffmanTreeBuildQueueHelper [] = PQueue.empty
huffmanTreeBuildQueueHelper (x:xs) = PQueue.insert p (Leaf p c) (huffmanTreeBuildQueueHelper xs)
  where
    (c, p) = x

mergeHuffmanTreeNodes :: HuffmanTree -> HuffmanTree -> (HuffmanTree, Int)
mergeHuffmanTreeNodes h1 h2 
  | w1 <= w2 = ((Branch t h1 h2), t)
  | otherwise = ((Branch t h2 h1), t)
  where w1 = getFreq h1
        w2 = getFreq h2
        t = w1+ w2

buildHuffmanFromQueue :: PQueue Int HuffmanTree -> HuffmanTree
buildHuffmanFromQueue q = case PQueue.extractMin q of
  -- Nothing -> error "should not happen"
  Just (q1, j1) -> case PQueue.extractMin q1 of 
    Nothing -> j1
    Just (q2, j2) -> buildHuffmanFromQueue (PQueue.insert t j q2)
      where (j, t) = mergeHuffmanTreeNodes j1 j2

huffmanTree :: [(Char, Int)] -> HuffmanTree
huffmanTree xs = buildHuffmanFromQueue (huffmanTreeBuildQueueHelper xs)

buildDictHelper :: HuffmanTree -> [Bool] -> Map Char [Bool] -> Map Char [Bool]
buildDictHelper (Leaf _ c) b m = Map.insert c b m
buildDictHelper (Branch f l r) b m = buildDictHelper r (b ++ [True]) lm where
  lm = buildDictHelper l (b ++ [False]) m

buildDict :: HuffmanTree -> Map Char [Bool]
buildDict h = buildDictHelper h [] Map.empty

encode :: HuffmanTree -> [Char] -> [Bool]
encode tree cs = concatMap (\c -> dict ! c) cs
  where
    dict = buildDict tree
