module Utilities exposing (..)

import EveryDict

grouped : List a -> List (List a)
grouped list =
  list 
    |> List.foldl
      (\item dict -> EveryDict.update
        item
        (insertOrAppend item)
        dict)
      EveryDict.empty
    |> EveryDict.values

flatten : List (List a) -> List a
flatten list =
  List.foldr (++) [] list

insertOrAppend : a -> Maybe (List a) -> Maybe (List a)
insertOrAppend item m = 
  case m of
    Just list -> Just (list ++ [item])
    Nothing -> Just [item]

filterNot : (a -> Bool) -> List a -> List a
filterNot predicate list =
  List.filter (\i -> not (predicate i)) list

appendToHeadOfSecond : a -> (List (List a), List (List a)) -> List (List a)
appendToHeadOfSecond x lists =
  let 
    first = Tuple.first lists
    rest = List.drop 1 (Tuple.second lists)
    appended = 
      case List.head (Tuple.second lists) of
        Just list -> [x::list]
        Nothing -> [[x]]
  in 
    first ++ appended ++ rest

-- https://stackoverflow.com/questions/35423903/haskell-all-possible-partitions-of-a-list
bloat : a -> List (List a) -> List (List (List a))
bloat x list =
  List.range 0 (List.length list)
    |> List.map (\i -> (List.take i list, List.drop i list))
    |> List.map (appendToHeadOfSecond x)

partitions : List a -> List (List (List a))
partitions list =
  let
    applyBloat = \n l -> l |> List.map (bloat n) |> flatten
  in  
    List.foldr applyBloat [[]] list

cartesianProduct : List (List a) -> List (List a)
cartesianProduct lists =
  case lists of 
    [] -> 
      [[]]
    x::xs -> 
      let
        applyToSubProduct = \k -> List.map ((::) k) (cartesianProduct xs)
      in
        List.map applyToSubProduct x |> flatten
