{-

The first data type defined in chapter 2 of PFDS.

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

-}
module PFDS.Stack
  (empty
  ,isEmpty
  ,cons
  ,head'
  ,tail'
  ,catenate
) where

type Stack a = [a]

empty :: Stack a
empty = [] 

isEmpty :: Stack a -> Bool
isEmpty [] = True
isEmpty _ = False

cons :: a -> Stack a -> Stack a
cons x xs = x : xs

head' :: Stack a -> a
head' [] = error "empty stack"
head' (x:_) = x

tail' :: Stack a -> [a]
tail' [] = error "empty stack"
tail' (_:xs) = xs

catenate :: Stack a -> Stack a -> Stack a
catenate [] [] = []
catenate [] (y:ys) = y : catenate [] ys
catenate (x:xs) y = x : catenate xs y

-- suffixes can simple create pointers to successive elements in the
-- list, and only has to iterate through the list to do so.
-- For example:
--     [1,2,3,4] ->[(pointer to list starting at 0), (pointer to list
--     starting at 1)], etc...
suffixes :: Stack a -> Stack (Stack a)
suffixes [] = cons empty empty
suffixes (x:xs) = cons (cons x xs) $ suffixes xs
