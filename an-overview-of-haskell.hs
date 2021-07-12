module BlockChain (
  chainLength
  hasBlock
) where

data Chain = 
    GenesisBlock
  | Block Chain Txs
type Txs = Int

chain1 = Block GenesisBlock 2

chain2 = Block chain1 4 

chainLength :: Chain -> Int
chainLength GenesisBlock = 0
chainLength (Block c _) = 1 + chainLength c

hasBlock :: Txs -> Chain -> Bool
hasBlock x GenesisBlock = False
hasBlock x (Block c t) = 
    x == t || hasBlock x c

(|>) :: Chain -> Txs -> Chain
(|>) = Block
infixl 5 |>

chain2'' :: Chain
chain2'' = GenesisBlock |> 2 |> 4