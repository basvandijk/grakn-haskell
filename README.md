A simple library for building and executing Graql queries.

Import the modules:

```haskell
module Example where

import Graql
import Graql.Shell

import Data.Function ((&))
```

Define the type names:

```haskell
person = name "person"
husband = name "husband"
wife = name "wife"
marriage = name "marriage"
```

Define the variables:

```haskell
x = var "x"
y = var "y"
```

We can translate the following query into Haskell:

```graql
match $x isa person, (husband: $x, wife: $y) isa marriage; select $y;
```

```haskell
query = match
    [ x `isa` person
    , rel [husband .: x, wife .: y] `isa` marriage
    ] & select [y]
```

We can also use infix functions like `(-:)` instead of `isa`:

```haskell
otherQuery = match
    [ x -: person
    , rel [husband .: x, wife .: y] -: marriage
    ] & select [y]
```

To execute and print the results of our query:

```haskell
main = do
    result <- runMatch query
    print result
```
