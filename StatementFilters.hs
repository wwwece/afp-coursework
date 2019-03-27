-- Filter-functions for filtering certain kinds of statements from the story.

module StatementFilters
( onlyPersonLocationStatements
, onlyPersonAndItemLocationStatements
, onlyItemCountStatements
, onlyDirectionStatements
) where

import Data.Char (toLower)

-- Vocabulary:
names      = ["sandra", "fred", "daniel", "bill", "mary", "john"] 
locations  = ["kitchen", "hallway", "bathroom", "garden", "bedroom", "office", "school", "park", "cinema"] 
items      = ["milk", "football", "apple"]
item_verbs = ["took", "got", "picked", "handed", "dropped", "discarded"]


onlyPersonLocationStatements :: String -> Bool
onlyPersonLocationStatements statement
  | (length $ words statement) > 1 = let stmt@(w1:_) = words $ map toLower statement 
                                     in w1 `elem` names && last stmt `elem` locations
  | otherwise = False


onlyPersonAndItemLocationStatements :: String -> Bool
onlyPersonAndItemLocationStatements statement
  | (length $ words statement) > 2 = let stmt@(w1:w2:_) = words $ map toLower statement 
                                     in  w1 `elem` names 
                                        && ((last stmt `elem` locations || last stmt `elem` items) 
                                           || w2 `elem` ["handed"])
  | otherwise = False


onlyItemCountStatements :: String -> Bool
onlyItemCountStatements statement 
  | (length $ words statement) > 2 = let stmt@(w1:w2:_) = words $ map toLower statement
                                     in  w1 `elem` names && w2 `elem` item_verbs 
  | otherwise = False


onlyDirectionStatements :: String -> Bool
onlyDirectionStatements statement 
  | (length $ words statement) > 2 = let stmt@(w1:w2:_) = words $ map toLower statement
                                     in  w1 == "the" && w2 `elem` locations && (last stmt `elem` locations)
  | otherwise = False
