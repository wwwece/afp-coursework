import System.Environment
import System.IO
import Data.Char (toLower)

main :: IO ()
main = do 
  story <- getStory
  prettyPrintList story
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
answerHowDoYouGo (_:_:_:_:_:_:location1:_:_:location2:_) story = "TODO (How do you go...)"


-- ----------------------- --
-- LITTLE HELPER FUNCTIONS --
-- ----------------------- --

getStory :: IO [String]
getStory = do
  args <- getArgs
  file <- readFile $ head args
  return $ lines file

prettyPrintList :: [String] -> IO ()
prettyPrintList xs = do putStrLn "\n> THE STORY:"; prettyPrintList' xs
  where prettyPrintList' []     = do putStrLn "> END OF STORY.\n"
        prettyPrintList' (x:xs) = do putStrLn ("  " ++ x)
                                     prettyPrintList' xs

maybe2string :: Show a => Maybe a -> String
maybe2string Nothing = "don't know"
maybe2string (Just x) = show x

maybeStr2string :: Maybe String -> String
maybeStr2string Nothing = "don't know"
maybeStr2string (Just x) = x

