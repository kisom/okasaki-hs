The first data type defined in chapter 2 of PFDS is the stack.

```
Its signature is defined as (in SML):
signature STACK = 
sig
  type a Stack
  
  val empty       :: a Stack
  val isEmpty     :: a Stack -> bool
  val cons        :: a x a Stack -> a Stack
  val head        :: a Stack -> a
  val tail        :: a Stack -> a Stack
end
```

A stack is implemented as a list.

> data Stack a = Empty | Cons a (Stack a)
 
It will be useful to print out lists:

> showStack :: (Show a) => Stack a -> String
> showStack Empty = ")"
> showStack (Cons x xs) = (show x) ++ " " ++ (showStack xs)
 
> instance (Show a) => Show (Stack a) where
>   show x = "( " ++ showStack x

Empty is already a value of the list, so implementing isEmpty only
requires determining whether the first argument is `Empty` or not.

> isEmpty :: Stack a -> Bool
> isEmpty Empty = True
> isEmpty _ = False

Consing should add an item to the front of the list.

> cons :: a -> Stack a -> Stack a
> cons x s = Cons x s

Head returns the first element in the list.

> shead :: Stack a -> a
> shead Empty = error "Empty stack"
> shead (Cons x _) = x
