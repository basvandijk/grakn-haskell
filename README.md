A simple library for building and executing Graql queries.

Import the modules:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Example where

import Grakn

import Data.Function ((&))
```

Define the type names:

```haskell
person :: Name
person = name "person"

husband :: Name
husband = name "husband"

wife :: Name
wife = name "wife"

marriage :: Name
marriage = name "marriage"
```

Define the variables:

```haskell
x :: Var
x = var "x"

y :: Var
y = var "y"
```

We can translate the following query into Haskell:

```graql
match $x isa person, (husband: $x, wife: $y) isa marriage; select $y;
```

```haskell
query :: MatchQuery
query = match
    [ x `isa` person
    , rel [husband .: x, wife .: y] `isa` marriage
    ] & select [y]
```

We can also use infix functions like `(-:)` instead of `isa`:

```haskell
otherQuery :: MatchQuery
otherQuery = match
    [ x -: person
    , rel [husband .: x, wife .: y] -: marriage
    ] & select [y]
```

To execute and print the results of our query:

```haskell
graph :: Graph
graph = Graph defaultUrl "my-keyspace"

main :: IO ()
main = do
    result <- execute graph query
    print result
```
