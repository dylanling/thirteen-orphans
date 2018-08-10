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

insertOrAppend : a -> Maybe (List a) -> Maybe (List a)
insertOrAppend item m = 
  case m of
    Just list -> Just (list ++ [item])
    Nothing -> Just [item]

filterNot : (a -> Bool) -> List a -> List a
filterNot predicate list =
  List.filter (\i -> not (predicate i)) list