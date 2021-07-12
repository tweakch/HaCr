{-# LANGUAGE DeriveFoldable #-}
-- import Data.Char 
-- digitalRoot :: Integral a => a -> a
-- digitalRoot 0 = 0
-- digitalRoot n = 1 + (n - 1) `mod` 9

-- digitalRootBase :: Integral a => (a,a) -> a
-- digitalRootBase (0,_) = 0
-- digitalRootBase (n,b) = 1 + (n - 1) `mod` (b - 1)

-- bracket' :: Int -> Char
-- bracket' 1 = '('
-- bracket' x = ')'

-- duplicateEncode :: String -> String
-- duplicateEncode str = [ bracket $ length $ filter (== toUpper x) (map toUpper str) | x <- str ] 
--     where bracket 1 = '('
--           bracket x = ')'

-- digs :: Integral a => a -> a
-- digs 0 = 0
-- digs x = x `mod` 10 + digs (x `div` 10)

data Chain txs = 
    GenesisBlock
  | Block (Chain txs) txs
  deriving (Eq, Show, Foldable)

chain1 = Block GenesisBlock 2

chain2 = Block chain1 4 

chainLength :: Chain txs -> Int
chainLength GenesisBlock = 0
chainLength (Block c _) = 1 + chainLength c

-- hasBlock :: txs -> Chain txs -> Bool
-- hasBlock x GenesisBlock = False
-- hasBlock x (Block c t) = 
--     x == t || hasBlock x c

(|>) :: Chain txs -> txs -> Chain txs
(|>) = Block
infixl 5 |>

-- chain2'' :: Chain txs
-- chain2'' = GenesisBlock |> 2 |> 4

hasBlockProp :: (txs -> Bool) -> Chain txs -> Bool
hasBlockProp prop GenesisBlock = False
hasBlockProp prop (Block c t) =
    prop t || hasBlockProp prop c