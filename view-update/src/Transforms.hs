module Transforms
  (
  ) where

import qualified Data.Map as M

data Tree
  = Tree
      { children :: M.Map [Char] Tree
      }
  | Empty
  deriving (Show, Eq)

getChild :: Tree -> [Char] -> Tree
getChild Empty _ = Empty
getChild (Tree {children = children}) key =
  case lookupRes of
    (Just res) -> res
    (Nothing) ->
      case (first
              (\t -> t /= Empty)
              [getChild c key | (_, c) <- M.toList children]) of
        (Just child) -> child
        (Nothing) -> Empty
  where
    lookupRes = M.lookup key children

mapTree :: ([Char] -> [Char]) -> Tree -> Tree
mapTree func Empty = Empty
mapTree func (Tree {children = children}) =
  Tree
    (M.fromList [(func key, (mapTree func c)) | (key, c) <- M.toList children])

testAbstractTree :: Tree
testAbstractTree =
  Tree
    (M.fromList
       [ ("Tomas", Tree (M.fromList [("01239349", Empty)]))
       , ("Dylan", Tree (M.fromList [("0871232131", Empty)]))
       ])

treeify :: [([Char], Tree)] -> Tree
treeify mappings = Tree (M.fromList mappings)

testConcreteTree :: Tree
testConcreteTree =
  treeify
    [ ( "Tomas"
      , treeify
          [ ("Phone", treeify [("087123456", Empty)])
          , ("DOB", treeify [("13/09/1898", Empty)])
          ])
    , ( "Sinead"
      , treeify
          [ ("Phone", treeify [("102302130", Empty)])
          , ("DOB", treeify [("14/07/1898", Empty)])
          ])
    ]

liftTree :: Tree -> Tree
liftTree t =
  case t of
    (Tree {children = children}) ->
      treeify
        [ (name, treeify [(number, Empty)])
        | (name, number) <- zip names numbers
        ]
      where names = [name | (name, _) <- M.toList children]
            numbers = [getNumber t name | name <- names]

putBackTree :: Tree -> Tree -> Tree
putBackTree modifiedAbstractTree oldConcreteTree =
  case (modifiedAbstractTree, oldConcreteTree) of
    ((Tree {children = abstractChildren}), (Tree {children = concreteChildren})) ->
      newConcreteTree
      where names = [k | (k, _) <- M.toList abstractChildren]
            numbers =
              [getNumberAbstract modifiedAbstractTree name | name <- names]
            dobs = [getDob oldConcreteTree name | name <- names]
            newConcreteTree =
              treeify
                [ ( name
                  , treeify
                      [ ("Phone", treeify [(number, Empty)])
                      , ("DOB", treeify [(dob, Empty)])
                      ])
                | (name, number, dob) <- zip3 names numbers dobs
                ]

getField :: Tree -> [Char] -> [Char] -> Maybe [Char]
getField t name field =
  case (getChild (getChild t name) field) of
    (Tree {children = children}) ->
      Just (head [k | (k, v) <- M.toList children])
    Empty -> Nothing

getNumberAbstract :: Tree -> [Char] -> [Char]
getNumberAbstract t name =
  case (getChild t name) of
    (Tree {children = children}) -> head [k | (k, v) <- M.toList children]
    Empty -> "00000000000"

getNumber :: Tree -> [Char] -> [Char]
getNumber t name =
  case (getField t name "Phone") of
    (Just number) -> number
    (Nothing) -> "0000000000"

getDob :: Tree -> [Char] -> [Char]
getDob t name =
  case (getField t name "DOB") of
    (Just dob) -> dob
    (Nothing) -> "00/00/0000"

first :: (Eq a) => (a -> Bool) -> [a] -> Maybe a
first _ [] = Nothing
first pred (a:as) =
  if (pred a)
    then (Just a)
    else (first pred as)
