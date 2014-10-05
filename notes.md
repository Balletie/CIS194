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

There is another way of polymorphism, called *ad-hoc polymorphism*, which
is used when you don't want a function to work for every type, but only for
some types. The function `(+)` is a good example: it should only have to
work for `Ints`, `Integer`s, `Double`s, etc.

*Ad-hoc polymorphism* is achieved in Haskell using **type classes**, for
which the `class` keyword is reserved:

```
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

Type classes define a set of operations that work on *class instances*.
A class instance is a type which defines these functions. For this, the
`instance` keyword is reserved:

```
Data Foo = F Int

instance Eq Foo where
  (F x) == (F y) = x == y
```

Type classes can also contain default implementations. For example, the
`(/=)` function of the `Eq` class is defined using the negation of `(==)`:

```
x /= y = not (x == y)
```

Another possibility is to do the following:

```
x == y = not (x /= y)
```

You can also define both in terms of the other, which is what the `Eq`
class does. Then you only have to define one of the two functions. If you
don't do that however, you get infinite recursion.

##Type classes and Java interfaces

There are some similarities between type classes and *Java interfaces*:
both define functions with either a default implementation, or no
implementation. Different types can implement these functions.

However, Haskell's type classes can also work on multiple type parameters:

```
class Blerg a b where
  blerg :: a -> b -> Bool
```
