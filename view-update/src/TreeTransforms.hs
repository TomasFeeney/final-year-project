module TreeTransforms
  (
  ) where

data Tree
  = Hoist Tree Tree
  | Plunge Tree Tree
  | Empty
