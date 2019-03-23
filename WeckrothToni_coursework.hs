
import System.Environment
import System.IO
import Data.Char (toLower)

main :: IO ()
main = do 
  story <- getStory
  putStrLn "\n> THE STORY:"
  prettyPrintList story
  askQuestions story


-- TODO: TYPE SIGNATURE
-- askQuestions :: Foldable t => t String -> IO b
askQuestions story = do
  putStr "ASK A QUESTION or (q)uit: "
  question <- getLine
  if map toLower question == "q" || map toLower question == "quit"
    then do return ()
    else do putStr $ question ++ " "
            answer (words question) story
            askQuestions story


-- TODO: TYPE SIGNATURE
-- answer :: Foldable t => [[Char]] -> t String -> IO ()
answer question@(word1:word2:_) story
  | w1 == "is"                    = putStrLn $ answerIs question story
  | w1 == "where" && w2 == "is"   = putStrLn $ answerWhereIs question story
  | w1 == "where" && w2 == "was"  = putStrLn $ answerWhereWas question story
  | w1 == "how"   && w2 == "many" = putStrLn $ maybe2string $ answerHowMany question story
  | w1 == "how"   && w2 == "do"   = putStrLn $ answerHowDoYouGo question story
  | otherwise                     = putStrLn "I don't understand!"
  where w1 = map toLower word1
        w2 = map toLower word2


-- Pattern in IS-questions: (is:name:in:the:location:?)
-- TODO: answerIs :: Foldable t => [String] -> t String -> Maybe Bool
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


-- TODO: TYPE SIGNATURE
-- Pattern in WHERE IS -questions (where:is:the:item:?)
answerWhereIs (_:_:_:item:_) story = 
  whereIsPerson (foldl (\person statement -> 
                       let s = words $ map toLower statement
                           item' = map toLower item
                       in  if item' `elem` s
                           then Just (head $ words statement) -- Name of the person posessing the item
                           else person
                ) Nothing story) story
  -- Checks where is a person with a given name
  where whereIsPerson Nothing     _     = "don't know"
        whereIsPerson (Just name) story = 
          foldl (\loc statement -> 
                let (person:verb:_) = words $ map toLower statement 
                    name' = map toLower name
                in  if name' == person && verb `elem` ["moved","went","journeyed"]
                    then last $ words statement
                    else loc
          ) "don't know" story
answerWhereIs _ _ = "don't know"


-- TODO: TYPE SIGNATURE
-- Pattern in WHERE WAS -questions (where:was:person:before/after:location:?)
answerWhereWas (_:_:person:time:location:_) story = "TODO (Where was...)"

-- TODO: TYPE SIGNATURE
-- Pattern in HOW MANY -questions (how:many:objects:is:person:carrying:?)
-- Verbs: took, got, discarded, picked & HANDED to
answerHowMany (_:_:_:_:person:_:_) story = 
  foldl (\answer statement -> 
    let stm@(name:verb:_:_:_) = words $ map toLower statement
        person' = map toLower person
        stmTail = last $ words $ map toLower statement
    in  if person' == name
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
prettyPrintList []     = do putStrLn "> END OF STORY.\n"
prettyPrintList (x:xs) = do putStrLn ("  " ++ x)
                            prettyPrintList xs

maybe2string :: Show a => Maybe a -> String
maybe2string Nothing = "don't know"
maybe2string (Just x) = show x








