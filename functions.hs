import Data.Char 
digitalRoot :: Integral a => a -> a
digitalRoot 0 = 0
digitalRoot n = 1 + (n - 1) `mod` 9

digitalRootBase :: Integral a => (a,a) -> a
digitalRootBase (0,_) = 0
digitalRootBase (n,b) = 1 + (n - 1) `mod` (b - 1)

bracket' :: Int -> Char
bracket' 1 = '('
bracket' x = ')'

duplicateEncode :: String -> String
duplicateEncode str = [ bracket $ length $ filter (== toUpper x) (map toUpper str) | x <- str ] 
    where bracket 1 = '('
          bracket x = ')'

digs :: Integral a => a -> a
digs 0 = 0
digs x = x `mod` 10 + digs (x `div` 10)