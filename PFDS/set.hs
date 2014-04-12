{-

The set data type defined in chapter 2 of PFDS, implemented as a tree.

Its signature (in SML) is defined as:

signature SET =
sig
  type Elem
  type Set

  val empty       :: Set
  val insert      :: Elem x Set -> Set
  val member      :: Elem x Set -> Bool
end

-}
module Set where

data Tree a = Empty | Tree { right :: Tree a
                             ,value :: a
                             ,left :: Tree a
} deriving Show

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty = False
member x t
  | x == value t = True
  | x < value t = member x $ right t
  | x > value t = member x $ left t

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Tree Empty x Empty
insert x t
  | x == value t = t
  | x < value t = Tree (insert x $ right t) (value t) (left t)
  | x > value t = Tree (right t) (value t) (insert x $ left t)
