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

import qualified Data.List as List

data Tree a = Empty | Tree { left :: Tree a
                             ,value :: a
                             ,right :: Tree a
} deriving Show

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty = False
member x (Tree l v r)
  | x == v = True
  | x < v = member x l
  | x > v = member x r

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Tree Empty x Empty
insert x (Tree l v r)
  | x == v = Tree l v r
  | x < v = Tree (insert x l) v r
  | x > v = Tree l v (insert x r)

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Empty
fromList x = treeList Empty x
  where treeList t [] = t
        treeList t (x:xs) = treeList (insert x t) xs
        
members :: (Ord a) => Tree a -> [a]
members t = List.sort $ unrollTree t
  where unrollTree Empty = []
        unrollTree (Tree l v r) = v : (unrollTree l) ++ (unrollTree r)
