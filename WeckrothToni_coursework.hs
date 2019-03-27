import System.Environment
import System.IO
import Data.Char (toLower)
import Data.List (intercalate)

           -- VOCABULARY:
names      = ["sandra", "fred", "daniel", "bill", "mary", "john"] 
locations  = ["kitchen", "hallway", "bathroom", "garden", "bedroom", "office", "school", "park", "cinema"] 
items      = ["milk", "football", "apple"]
item_verbs = ["took", "got", "picked", "handed", "dropped", "discarded"]


main :: IO ()
main = do 
  story <- getStory
  prettyPrintStory story
  askQuestions story


askQuestions :: [String] -> IO ()
askQuestions story = do
  putStr "ASK A QUESTION or (q)uit: "
  question <- getLine
  if map toLower question == "q" || map toLower question == "quit"
    then do return ()
    else do putStr $ question ++ " "
            answer (words question) story
            askQuestions story


answer :: [String] -> [String] -> IO ()
answer question@(word1:word2:_) story
  | w1 == "is"                    = putStrLn $ answerIs question (filter onlyPersonLocationStatements story)
  | w1 == "where" && w2 == "is"   = putStrLn $ maybeStr2string $ answerWhereIs question (filter onlyPersonAndItemLocationStatements story)
  | w1 == "where" && w2 == "was"  = putStrLn $ maybeStr2string $ answerWhereWas question (filter onlyPersonLocationStatements story)
  | w1 == "how"   && w2 == "many" = putStrLn $ maybe2string $ answerHowMany question (filter onlyItemCountStatements story)
  | w1 == "how"   && w2 == "do"   = putStrLn $ answerHowDoYouGo question (filter onlyDirectionStatements story)
  | otherwise                     = putStrLn "I don't understand!"
  where w1 = map toLower word1
        w2 = map toLower word2


onlyPersonLocationStatements :: String -> Bool
onlyPersonLocationStatements xs
  | (length $ words xs) > 1 = let stmt@(w1:_) = words $ map toLower xs 
                              in w1 `elem` names && last stmt `elem` locations
  | otherwise = False

onlyPersonAndItemLocationStatements :: String -> Bool
onlyPersonAndItemLocationStatements xs
  | (length $ words xs) > 2 = let stmt@(w1:w2:_) = words $ map toLower xs 
                              in  w1 `elem` names 
                                    && (last stmt `elem` locations || last stmt `elem` items) 
                                      || w2 `elem` ["handed"]
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


-- Pattern in IS-questions: (is:name:in:the:location:?)
answerIs :: Foldable t => [String] -> t String -> String
answerIs (_:name:_:_:location:_) story = 
  foldl (\answer statement -> let statement' = (words $ map toLower statement) in
        if (map toLower name) `elem` statement'
        then checkConditions statement' (map toLower location) answer
        else answer
  ) "maybe" story
  where checkConditions stmt loc answer
          | loc `elem` stmt && not ("no" `elem` stmt) && not ("either" `elem` stmt) = "yes"
          | ("no" `elem` stmt && loc `elem` stmt)                                   = "no"
          | (not ("no" `elem` stmt) && not ("either" `elem` stmt))                  = "no"
          | "either" `elem` stmt && not (loc `elem` stmt)                           = "no"
          | "either" `elem` stmt && loc `elem` stmt                                 = "maybe"
          | "no" `elem` stmt && not (loc `elem` stmt)                               = "maybe"
          | otherwise = answer
answerIs _ _ = "maybe"


-- Pattern in WHERE IS -questions (where:is:the:item:?)
-- Verbs: moved, went, journeyed
answerWhereIs :: Foldable t => [String] -> t String -> Maybe String
answerWhereIs (_:_:_:item:_) story = 
  whereIsPerson (foldl (\person statement -> 
                       let s@(_:verb:_) = words $ map toLower statement
                           item' = map toLower item
                       in  if item' `elem` s
                           then if verb `elem` ["handed"] 
                                then Just (last $ words statement) -- Name of the person posessing the item
                                else Just (head $ words statement) -- Name of the person posessing the item
                           else person
                ) Nothing story) story
  where whereIsPerson Nothing     _     = Nothing
        whereIsPerson (Just name) story = 
          foldl (\loc statement -> 
                let (person:verb:_) = words $ map toLower statement 
                    name' = map toLower name
                in  if name' == person && verb `elem` ["moved","went","journeyed"]
                    then Just (last $ words statement)
                    else loc
          ) Nothing story
answerWhereIs _ _ = Nothing


-- Pattern in WHERE WAS -questions (where:was:person:before/after:the:location:?)
answerWhereWas :: Foldable t => [String] -> t String -> Maybe String
answerWhereWas (_:_:person:time:_:location:_) story = 
  if time `elem` ["before"]
  then snd (foldl (\answer statement -> whereWas statement answer) (False, Nothing) story)
  else if time `elem` ["after"]
       then snd (foldr (\statement answer -> whereWas statement answer) (False, Nothing) story)
       else Nothing
  where whereWas statement (placeWasFound,answer) =
          if placeWasFound == True
          then (placeWasFound, answer)
          else if answer == (Just "don't know") 
               then (placeWasFound, answer) 
               else let (subject:verb:_:_:place:_) = words $ map toLower statement 
                        person' = map toLower person
                    in if person' == subject
                       then if location == place
                            then if answer == Nothing
                                 then (True, Just "don't know")
                                 else (True, answer)
                            else (placeWasFound, Just place)
                       else (placeWasFound, answer)


-- Pattern in HOW MANY -questions (how:many:objects:is:person:carrying:?)
-- Verbs: took, got, discarded, picked & HANDED to
answerHowMany :: (Foldable t, Num b, Eq b) => [String] -> t String -> Maybe b
answerHowMany (_:_:_:_:person:_:_) story = 
  foldl (\answer statement -> 
    let (subject:verb:_:_:_) = words $ map toLower statement
        person' = map toLower person
        stmTail = last $ words $ map toLower statement
    in  if person' == subject
        then countObject answer verb
        else if verb `elem` ["handed"] && person' == stmTail
             then if answer == Nothing 
                  then Just 1
                  else Just (+1) <*> answer
             else answer
  ) Nothing story
  where countObject answer verb 
          | answer == Nothing                             = countObject (Just 0) verb
          | verb `elem` ["took","got","picked"]           = Just (+1)         <*> answer
          | verb `elem` ["discarded","dropped","handed"]  = Just (subtract 1) <*> answer
          | otherwise = answer
answerHowMany _ _ = Nothing


-- Pattern in HOW DO YOU GO -questions (how:do:you:go:from:the:location:to:the:location:?)
answerHowDoYouGo :: Foldable t => [String] -> t String -> String
answerHowDoYouGo (_:_:_:_:_:_:initialStartLoc:_:_:initialEndLoc:_) story = 
  intercalate ", " $ map (\(_,d) -> d) $ tail $ howToGo initialStartLoc initialEndLoc story [(initialStartLoc, "INIT")]
  where howToGo startLoc endLoc story answer = 
          foldl (\answer statement ->
            if fst (last answer) == "PATH_FOUND!"
            then answer
            else let (_:storyLoc1:_:direction:_:_:storyLoc2:_) = words $ map toLower statement 
                     visitedPlaces = map (\(place, _) -> place) answer
                     -- lastVisitedPlace = fst $ last answer
                 in  if startLoc == storyLoc1
                     then if storyLoc2 == endLoc
                          then answer ++ [("PATH_FOUND!", opposite direction)]
                          else if storyLoc2 `elem` visitedPlaces
                               then if storyLoc2 == initialStartLoc
                                    -- This is not entirely working solution because of this if-then-else-block.
                                    -- Comparision above is done against initialStartLoc and this results so that 
                                    -- some correct "moves" that come after the first move might be forgetted 
                                    -- from the path. Couldn't deduce when to return all of the answer-items 
                                    -- ("directions") and when the last one can be forgetted (dead-end so to speak).
                                    -- This passes the given tests though...
                                    then (answer)
                                    else (init answer)
                               else howToGo storyLoc2 endLoc story (answer ++ [(storyLoc1, opposite direction)])
                     else if startLoc == storyLoc2 
                          then if storyLoc1 == endLoc
                               then answer ++ [("PATH_FOUND!", direction)]
                               else if storyLoc1 `elem` visitedPlaces 
                                    then if storyLoc1 == initialStartLoc
                                         -- Look at the above comment, same goes in this block as well.
                                         then (answer)
                                         else (init answer)
                                    else howToGo storyLoc1 endLoc story (answer ++ [(storyLoc2, direction)])
                          else answer            
          ) answer story
answerHowDoYouGo _ _ = "don't know"


opposite :: String -> String
opposite "west"  = "east"
opposite "east"  = "west"
opposite "north" = "south"
opposite "south" = "north"
opposite x       = x


-- ----------------------- --
-- LITTLE HELPER FUNCTIONS --
-- ----------------------- --

getStory :: IO [String]
getStory = do
  args <- getArgs
  file <- readFile $ head args
  return $ lines file

prettyPrintStory :: [String] -> IO ()
prettyPrintStory xs = do putStrLn "\n> THE STORY:"; prettyPrintStory' xs
  where prettyPrintStory' []     = do putStrLn "> END OF STORY.\n"
        prettyPrintStory' (x:xs) = do putStrLn ("  " ++ x)
                                      prettyPrintStory' xs

maybe2string :: Show a => Maybe a -> String
maybe2string Nothing = "don't know"
maybe2string (Just x) = show x

maybeStr2string :: Maybe String -> String
maybeStr2string Nothing = "don't know"
maybeStr2string (Just x) = x
