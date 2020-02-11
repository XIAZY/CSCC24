module HuffmanDef where

data HuffmanTree = Leaf Int Char | Branch Int HuffmanTree HuffmanTree
    deriving (Eq, Show)

getFreq :: HuffmanTree -> Int
getFreq (Leaf f _) = f
getFreq (Branch f _ _) = f

