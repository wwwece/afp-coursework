module StatementFilters
( onlyPersonLocationStatements
, onlyPersonAndItemLocationStatements
, onlyItemCountStatements
, onlyDirectionStatements
) where

import Data.Char (toLower)

           -- VOCABULARY:
names      = ["sandra", "fred", "daniel", "bill", "mary", "john"] 
locations  = ["kitchen", "hallway", "bathroom", "garden", "bedroom", "office", "school", "park", "cinema"] 
items      = ["milk", "football", "apple"]
item_verbs = ["took", "got", "picked", "handed", "dropped", "discarded"]


onlyPersonLocationStatements :: String -> Bool
onlyPersonLocationStatements xs
  | (length $ words xs) > 1 = let stmt@(w1:_) = words $ map toLower xs 
                              in w1 `elem` names && last stmt `elem` locations
  | otherwise = False


onlyPersonAndItemLocationStatements :: String -> Bool
onlyPersonAndItemLocationStatements xs
  | (length $ words xs) > 2 = let stmt@(w1:w2:_) = words $ map toLower xs 
                              in  w1 `elem` names 
                                    && ((last stmt `elem` locations || last stmt `elem` items) 
                                      || w2 `elem` ["handed"])
  | otherwise = False


onlyItemCountStatements :: String -> Bool
onlyItemCountStatements xs 
  | (length $ words xs) > 2 = let stmt@(w1:w2:_) = words $ map toLower xs
                              in  w1 `elem` names && w2 `elem` item_verbs 
  | otherwise = False


onlyDirectionStatements :: String -> Bool
onlyDirectionStatements xs 
  | (length $ words xs) > 2 = let stmt@(w1:w2:_) = words $ map toLower xs
                              in  w1 == "the" && w2 `elem` locations && (last stmt `elem` locations)
  | otherwise = False
