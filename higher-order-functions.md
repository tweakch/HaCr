# 1.3 Higher Order Functions

* [Previous](an-overview-of-haskell.md)
* [Watch Lecture](https://www.youtube.com/watch?v=vaJ04672ke8&list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm&index=3)
* [Next](datatypes-and-functions.md)

## Higher Order Functions

### Finding a block with a property

Remember the `hasBlock` function?

```hs
hasBlock :: Txs -> Chain -> Bool
hasBlock x GenesisBlock = False
hasBlock x (Block c t) = 
    x == t || hasBlock x c
```

It checked if any `Block` in a given `Chain c` had a *specific* `Txs t`.

Let's suppose we want to check for more general *properties* like:

* is there a block greater than 10
* is there a block equal to 4
* is there a block with an even number
* is there a böock that is divisible by 7

We can model a *property* as a function of type `Txs -> Bool`

```hs
hasBlockProp :: (Txs -> Bool) -> Chain -> Bool
hasBlockProp prop GenesisBlock = False
hasBlockProp prop (Block c t) =
    prop t || hasBlockProp prop c
```

This function is called a *Higher Order Function* because it takes a function `Txs -> Bool` as a parameter. It's not a *curried function* because of the `()` parentheses.

#### Terminology

The above code has the following terminology:

|Term|Code|Explanation|
|---------------------------|----------------------------------|----------------------------------------------------------------------|
|function definition        | `hasBlockProp`                   | `::` reads as *"is of type"*                                         |
|function signature         | `(Txs -> Bool) -> Chain -> Bool` | the signature of the function                                        |
|function argument 1        | `(Txs -> Bool)`                  | first parameter is a *function*. We can think of it as a *property*. |
|function argument 2        | `Chain`                          | second parameter is a `Chain`                                        |
|funcion pattern 1          | `hasBlockProp prop GenesisBlock` | matches if the second function argument is a `GenesisBlock` [see definition](an-overview-of-haskell.md/#datatypes-and-functions)          |
|body of function pattern 1 | `= False`                        | returns `False` if the second function argument is a `GenesisBlock` [see definition](an-overview-of-haskell.md/#datatypes-and-functions) |
|funcion pattern 2          | `hasBlockProp prop (Block c t)`  | matches if the second function argument is a `(Block c t)` [see definition](an-overview-of-haskell.md/#datatypes-and-functions) |
|body of function pattern 2 | `prop t \|\| hasBlockProp prop c` | checks the current `Txs` for the *property* or calls `hasBlockProp` with `prop` and the parent `Chain c`|

### Anonymous functions and operator sections

Using *anonymous functions* also called *lambdas*.

```hs
ghci> hasBlockProp (\ x -> x > 10) chain2
False
ghci> hasBlockProp even chain2           
True
ghci> hasBlockProp (\x -> x == 4) chain2
True
ghci> hasBlockProp (\x -> x `mod` 7 == 0) chain2
False
```

or using *operator sections*

```hs
ghci> hasBlockProp (> 10) chain2
False
ghci> hasBlockProp (4 ==) chain2
True
ghci> hasBlockProp (== 4) chain2
True
```

#### Terminology

The above code has the following terminology:

|Term|Code|Explanation|
|-|-|-|
|lambda|`(\ x -> x > 10)`|"Inline function definition"|
|operator section|`(> 10)`|"simplified lambda"|

### Several styles to write one function

```hs
hasBlockProp :: (Txs -> Bool) -> Chain -> Bool
hasBlockProp prop GenesisBlock = False
hasBlockProp prop (Block c t) =
    prop t || hasBlockProp prop c
```

vs.

```hs
hasBlockProp :: (Txs -> Bool) -> Chain -> Bool
hasBlockProp = \ prop chain -> 
    case chain of
        GenesisBlock -> False
        Block c t    -> prop t || hasBlockProp prop c
```

### Similarity of function or type-directed programming

We have definded three functions so far:

```hs
chainLength :: Chain -> Int
chainLength GenesisBlock = 0
chainLength (Block c _)  = 1 + chainLength c
```

```hs
hasBlock :: Txs -> Chain -> Bool
hasBlock x GenesisBlock = False
hasBlock x (Block c t)  = 
    x == t || hasBlock x c
```

```hs
hasBlockProp :: (Txs -> Bool) -> Chain -> Bool
hasBlockProp prop GenesisBlock = False
hasBlockProp prop (Block c t) = 
    prop t || hasBlockProp prop c
```

Notice how every function has a similar structure:

* All take a `Chain` as an argument
* All have two bodies, one handling `GenesisBlock` and another one handling the type `(Block c t)`
* All bodies for `(Block c t)` contain a recursive call on their function

This is not a coinsidence. They all have this structure because of the data type `Chain` they operate on. Let's look at the `Chain` data type again and notice that - like the functions having two bodies - it has two *constructors*:

* the `GenesisBlock` constructor, with no arguments
* the `Block` constructor, with arguments `Chain` and `Txs`
* the `Chain` argument is a recursion on the `Chain` constructors

```hs
data Chain
    GenesisBlock
  | Block Chain Txs
type Txs = Int
```

A very useful thing to keep in mind is that **the structure of functions often follow the structure of the data type they are operating on**

## Types, overloading and polymorphism

### Type inference

As we've seen in the previous lecture ([here](an-overview-of-haskell.md/#determine-the-length-of-a-block-chain)), because Haskell has type inference, the *type signature* of a function is optional.

Let's look at `hasBlockProp` to see how Haskell would *infer* the type of a function if we where to omit it (which we shouldn't because it's good practice to specify it).

```hs
hasBlockProp :: 
hasBlockProp prop GenesisBlock = False
hasBlockProp prop (Block c t)  =
    prop t || hasBlockProp prop c
```

Haskell sees that each equation of `hasBlockProp` has *two arguments*. From this *shape* it can infer:

* `.. -> .. -> ..`

Furthermore it sees that one equation returns `False`. And `False` is a data constructor of the type `Bool`.

```hs
ghci>:i False
type Bool :: *
data Bool = False | ...
        -- Defined in `GHC.Types'
```

So Haskell can infer the *return type* of the function to be `Bool`.

* `.. -> .. -> Bool`

Likewise, it sees that the pattern for the *second arg* in first case is `GenesisBlock` which is a data constructor for the `Chain` type.

```hs
ghci> :i GenesisBlock
type Chain :: *
data Chain = GenesisBlock | ...
        -- Defined at functions.hs:24:5
```

Also the pattern for the *second arg* in second case `(Block c t)` matches one of the constructors of `Chain` namely `Block Chain Txs`.

```hs
ghci> :i Chain
type Chain :: *
data Chain = GenesisBlock | Block Chain Txs
        -- Defined at functions.hs:23:1
```

So Haskell can infer the *data type* of the second arg to be of type `Chain`.

* `.. -> Chain -> Bool`

It now sees that `t :: Txs` due to the tyoe of `Block`. And because `t` is applied to `prop` and `prop` is the first arg of each case it can infer `prop :: Txs -> ..`.

* `(Txs -> ..) -> Chain -> Bool`

Finally, due to `prop t || ...` we know that the result of `prop` is `Bool` because `(||)` is an operator to two `Bool` that returns `Bool`.

```hs
ghci> :t (||)
(||) :: Bool -> Bool -> Bool
```

This *very informal* rundown can be done by yourself and is key to become a productive Haskell programmer. So one should do lots of `:t` and `:i` to develop an intuition for what works.

### Parametric Polymorphism

Let's again look our `Chain` data type

```hs
data Chain = 
    GenesisBlock
  | Block Chain Txs
type Txs = Int
```

We can introduce a *type variable* to our `Chain` datatype to make it more abstract.

```hs
data Chain txs = 
    GenesisBlock
  | Block (Chain txs) txs
```

Now the constructors of `Chain` become **polymorphic** and include *type variables*

```hs
ghci> :t Block
Block :: Chain txs -> txs -> Chain txs
```

Also our functions are polymorphic now and need to take the *type variables* as arguments.

```hs
ghci> :t chainLength
chainLength :: Chain txs -> Int

ghci> :t hasBlockProp
hasBlockProp :: (txs -> Bool) -> Chain txs -> Bool
```

These work for any choice of type `txs`!

### Overloading

The `hasBlock` function is not entirely independent of type `t` and `x`.

```hs
hasBlock x GenesisBlock = False
hasBlock x (Block c t)  = 
    x == t || hasBlock x c
```

It's use of `(==)` constrains `x` and `t` to the `Eq` type class.

```hs
ghci> :t (==)
(==) :: Eq a => a -> a -> Bool -- reads (==) is of type a to a to Bool, as long as a is an instance of Eq.
```

```hs
{-# LANGUAGE DeriveFoldable #-}

data Chain txs = 
    GenesisBlock
  | Block (Chain txs) txs
  deriving (Eq, Show, Foldable)
``` 

## Summary

* higher order function definiton is a feature of functional languages
* polymorphism, data types and pattern matching are common to statically typed functional languages
* type classes are rather unique to Haskell
* Explicit effects and lazy evaluation make Haskell truly special