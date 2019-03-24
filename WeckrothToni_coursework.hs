import System.Environment
import System.IO
import Data.Char (toLower)
import Data.List (intercalate)

main :: IO ()
main = do 
  story <- getStory
  prettyPrintStory story
  askQuestions story


askQuestions :: Foldable t => t String -> IO ()
askQuestions story = do
  putStr "ASK A QUESTION or (q)uit: "
  question <- getLine
  if map toLower question == "q" || map toLower question == "quit"
    then do return ()
    else do putStr $ question ++ " "
            answer (words question) story
            askQuestions story


answer :: Foldable t => [String] -> t String -> IO ()
answer question@(word1:word2:_) story
  | w1 == "is"                    = putStrLn $ answerIs question story
  | w1 == "where" && w2 == "is"   = putStrLn $ maybeStr2string $ answerWhereIs question story
  | w1 == "where" && w2 == "was"  = putStrLn $ maybeStr2string $ answerWhereWas question story
  | w1 == "how"   && w2 == "many" = putStrLn $ maybe2string $ answerHowMany question story
  | w1 == "how"   && w2 == "do"   = putStrLn $ answerHowDoYouGo question story
  | otherwise                     = putStrLn "I don't understand!"
  where w1 = map toLower word1
        w2 = map toLower word2


-- Pattern in IS-questions: (is:name:in:the:location:?)
answerIs :: Foldable t => [String] -> t String -> String
answerIs (_:name:_:_:location:_) story = 
  foldl (\answer statement -> 
        let s     = words $ map toLower statement
            name' = map toLower name 
            loc   = map toLower location
        in  if name' `elem` s
            then if loc `elem` s && not ("no" `elem` s) && not ("either" `elem` s)
                 then "yes" -- location found without negation or either statement
                 else if ("no" `elem` s && loc `elem` s) || (not ("no" `elem` s) && not ("either" `elem` s))
                      then "no" -- not in this particular location OR in some other location
                      else if "no" `elem` s && not (loc `elem` s)
                           then "maybe" -- No longer in some other location, so can possibly be in this location.
                           else if "either" `elem` s && loc `elem` s
                                then "maybe" -- Is either in this location, or some other location
                                else answer
            else answer
  ) "maybe" story
answerIs _ _ = "maybe"


-- Pattern in WHERE IS -questions (where:is:the:item:?)
-- Verbs: moved, went, journeyed
answerWhereIs :: Foldable t => [String] -> t String -> Maybe String
answerWhereIs (_:_:_:item:_) story = 
  whereIsPerson (foldl (\person statement -> 
                       let s = words $ map toLower statement
                           item' = map toLower item
                       in  if item' `elem` s
                           then Just (head $ words statement) -- Name of the person posessing the item
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
  then (foldl (\answer statement -> whereWas statement answer) Nothing story)
  else if time `elem` ["after"]
       then (foldr (\statement answer -> whereWas statement answer) Nothing story)
       else Nothing
  where whereWas statement answer =
          if answer == (Just "don't know") 
          then (Just "don't know") 
          else let (subject:verb:_:_:place:_) = words $ map toLower statement 
                   person' = map toLower person
               in if person' == subject
                  then if location == place
                       then if answer == Nothing
                            then Just "don't know"
                            else answer
                       else Just place
                  else answer
answerWhereWas _ _ = Nothing



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
  where countObject Nothing verb = countObject (Just 0) verb
        countObject answer  verb = 
          if verb `elem` ["discarded"] 
          then Just (subtract 1) <*> answer
          else if verb `elem` ["took", "got", "picked"]
               then Just (+1) <*> answer
               else if verb `elem` ["handed"]
                    then Just (subtract 1) <*> answer
                    else answer
answerHowMany _ _ = Nothing


-- TODO: TYPE SIGNATURE
-- Pattern in HOW DO YOU GO -questions (how:do:you:go:from:the:location:to:the:location:?)
-- answerHowDoYouGo :: (Foldable t) => [String] -> t [Char] -> [(String, [Char])] -> [Char]
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
                                         -- Look at comments above, same goes in here as well.
                                         then (answer)
                                         else (init answer)
                                    else howToGo storyLoc1 endLoc story (answer ++ [(storyLoc2, direction)])
                          else answer            
          ) answer story


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

