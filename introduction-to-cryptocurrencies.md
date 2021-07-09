# 1.1 Introduction to Cryptocurrencies

[Lecture](https://www.youtube.com/watch?v=EoO76YCSTLo&list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm&index=1)
[Next](an-overview-of-haskell.md)

```hs
--digitalRoot = until(<10)$sum.map(read.(:"")).show
--                                           show  -- convert int to string
--                           map(         ).      -- turn each char (digit) into
--                                    pure        --    a string 
--                               read.            --    and then a number
--                       sum.                     -- sum up the list of numbers
--            until(<10)$                         -- repeat until the result is < 10
```