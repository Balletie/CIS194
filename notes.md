Algebraic Types
===============

Use the `data` keyword.

Can be used for **Enumeration types**:

```
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
deriving Show           -- Used for converting to Strings (magic for now)
```

You can do pattern matching on types, like so:

```
isSmall :: Thing -> Bool
isSmall Shoe    = True
isSmall Ship    = True
etc..
```

You can also have more complicated types, which use **data constructors**:

```
data Node = Leaf
          | Parent Node Value Node
  deriving (Show, Eq)
```

This is a data type which represents a Node, which can be either a Leaf
Node or a Parent Node. It is an example of a **recursive data type**,
useful for making binary (search) trees.

##Pattern-matching

Pattern-matching is also straightforward with this kind of data types:

```
foo Leaf = ...
foo (Parent _ value _) = ...
foo (Parent left value right) = ...
foo x@(Parent _ value Leaf) = ...
```

An underscore is a *don't care* variable, which matches anything.
A pattern like the one in the last line instantiates `x` with the entire
value that was matched. Patterns can also be nested, like you can see in
the last line.

Polymorphism
============

##Polymorphic data types

Same as normal data types, but also has a *type variable*. Take `Maybe`
as an example:

```
data Maybe a = Just a
             | Nothing
```

Now, you can substitute `a` with any type you would like. You could have
a `Maybe Int` for example, but also a `Maybe Node` or `Maybe Thing`.

Another example:

```
data List t = Empty | Cons t (List t)
```

##Polymorphic functions

They're functions that can act on polymorphic types:

```
safeHead :: List a -> Maybe a
safeHead Empty      = Nothing
safeHead (Cons x _) = Just x
```

The function `safeHead` works on any `List` with elements of type `a`.

Type classes
============

Use the `class` keyword.
