# 1.2 On Overview Of Haskell

[Previous](introduction-to-cryptocurrencies.md)
[Lecture](https://www.youtube.com/watch?v=ctfZ6DwFiPg&list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm&index=3)
[Next](higher-order-functions.md)

## Haskell History

Designed by a committee and succeeded to create a standard *lazy and functional* language

1990: First version in 1990, and very active development of initial standardization

1998: Realse

2010: Minor revision Haskell

Development outside of the standard, Glasgow Haskell Compiler (GHC)

Standardization effort has stalled

## Haskell Features

### fuctional

F#, Scala

### statically typed

F#, Scala

### algebraic datatypes

examples in later lectures

### type inference and polymorphism (Damas-Hindley-Milner type system)

don't have to specify the types you use

good compromise between good typesystem and remove burden from developer to get everything correct

polymorphism

### type classes

Not objects, more like interfaces and helps reuse of code

### explicit effects (usp)

Often called *pure*. Pure language, No side effects

IO, Networking, etc is specifically tagged in the typesystem

### lazy evalutaion (usp)

sparked development of haskell

not the most decisive feature, but still makes it unique

## Datatypes and functions

### A type for "block chains"

```hs
data Chain = 
    GenesisBlock
  | Block Chain Txs
type Txs = Int
```

#### Terminology

The above code has the following terminology:

|Term|Code|Explanation|
|-|-|-|
|datatype|`Chain`, `Int`|`Chain` is a new datatype, `Int` references already existing type|
|(data) constructor|`GenesisBlock`, `Block`| Ways to create values of that datatype. `GenesisBlock` |
|constructor arguments / fields |`Chain`, `Txs`| `GenesisBlock` constructor has no arguments. `Block` has two. |
|type synonym |`Txs` | Simplification happens here! `Txs` is just an `Int` |

### Example chains

The `Chain` datatype has two constructors and is recursive:

```hs
chain1 = Block GenesisBlock 2

chain2 = Block chain1 4 

chain2' = Block (Block GenesisBlock 2) 4
```

#### Terminology

|Term|Code|Explanation|
|-|-|-|
|binding|`chain1 = Block GenesisBlock 2`| Presence of `=` makes this line a binding |
|left hand side|`chain1` | Identifier of the binding |
|right hand side| `Block GenesisBlock 2` | on the right side of `=` |
|expressions| nested function applications and constants | the things that happen on the right hand side => everything is an expression |

### Determine the length of a block chain

```hs
1 chainLength :: Chain -> Int
2 chainLength GenesisBlock = 0
3 chainLength (Block c _) = 1 + chainLenght c
```

#### Terminology

|Line|Term|Code|Explanation|
|-|-|-|-|
|1|type signature|`chain1 = Block GenesisBlock 2`| Presence of `=` makes this line a binding |
|2 + 3|equations / cases|`chainLength GenesisBlock = 0`| Presence of `=` makes this line a binding |
|2 + 3|left hand sides |`chainLength GenesisBlock`, `chainLength (Block c _)` | Presence of `=` makes this line a binding |
|2 + 3|patterns |`GenesisBlock`, `(Block c _)` | Presence of `=` makes this line a binding |
|3|recursive call |`chainLength c` | Presence of `=` makes this line a binding |

### Evaluation step by step

Symbolic reduction

```hs
    chainLength chain2
  = chainLength (Block chain1 4)
  = 1 + chainLength chain1
  = 1 + chainLength (Block GenesisBlock 2)
  = 1 + (1 + chainLength GenesisBlock)
  = 1 + (1 + 0)
  = 1 + 1
  = 2
```

another example

```hs
  head $ tail [2,3,4]
= head $ tail 2:[3,4]
= head $ [3,4]
= head [3,4]
= 3
```

Also known as *equational reasoning*.

## Currying

### Testing for a particular block

```hs
hasBlock x GenesisBlock = False
hasBlock x (Block c t) = 
    x == t || hasBlock x c
```

The operator `||` implements a *logical disjunction ("or")*

> *Notice* that we did not add the type signature for `hasBlock` in the code above.

We can infer the type of `hasBlock` using GHCi.

```hs
ghci> :t hasBlock
hasBlock :: Txs -> Chain -> Bool
```

`::` reads as *"is of type"* so the signature reads: *"hasBlock is of type Txs to Chain to Bool"*

The repeated use of `->` in the type signature is called *currying*.

```hs
:t  hasBlock           :: Txs -> (Chain -> Bool)
:t  hasBlock 4         ::         Chain -> Bool
:t (hasBlock 4) chain2 ::                  Bool
```

We can look at `hasBlock` and say: "If you supply `Txs` to it, it will return another function of type `Chain -> Bool`.

Function arrows are implicitly nested to the right

Function application syntax is implicitly applied to the left

There is only one function type in Haskell a function that takes one argument and returns another. And since a function can return another function as a result you can simulate functions that take multiple parameters.

### Curried functions and operators

```hs
ghci> :t (||)
(||) :: Bool -> Bool -> Bool -- reads "curried Bool to Bool to Bool"
ghci> :t (&&)
(&&) :: Bool -> Bool -> Bool
ghci> :t Block
Block :: Chain -> Txs -> Chain
```

An operator is just a function that takes two arguments.

```hs
True || False    -- symbolic infix
(||) True False  -- symbolic prefix
Block chain1 4   -- alohanumeric prefix
chain1 `Block` 4 -- alphanumeric infix
```

### Operator priorities

```hs
ghci> :i (||)
(||) :: Bool -> Bool -> Bool    -- Defined in `GHC.Classes'
infixr 2 ||
```

We can define our own operators.

```hs
(|>) :: Chain -> Txs -> Chain
(|>) = Block
infixl 5 |>
```

And initialize a new `chain2''` using it.

```hs
chain2'' :: Chain
chain2'' = GenesisBlock |> 2 |> 4
```

This is equivalent to

```hs
ghci> hasBlock 4 chain2''
True
```

#### Left vs right associative

What is 5 - 2 - 2?

```hs
5 - (2 - 2) -- right associative
(5 - 2) - 2 -- left associative
```

```hs
ghci>  :i (-)                                      
type Num :: * -> Constraint
class Num a where
  ...
  (-) :: a -> a -> a
  ...
        -- Defined in `GHC.Num'
infixl 6 -
```

`(-)` is defined as `infixl` so it is left associative.

```hs
ghci> (5 - 2) - 2 == 5 - 2 - 2
True
```
