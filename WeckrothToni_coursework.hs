
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
  putStr "ASK A QUESTION: "
  question <- getLine
  putStr $ question ++ " "
  answer (words question) story
  askQuestions story


-- TODO: TYPE SIGNATURE
-- answer :: Foldable t => [[Char]] -> t String -> IO ()
answer question@(word1:word2:_) story
  | w1 == "is"                     = putStrLn $ answerIs question story
  | w1 == "where" && w2 == "is"   = putStrLn $ answerWhereIs question story
  | w1 == "where" && w2 == "was"  = putStrLn $ word1 ++ word2
  | w1 == "how"   && w2 == "many" = putStrLn $ word1 ++ word2
  | w1 == "how"   && w2 == "do"   = putStrLn $ word1 ++ word2
  | otherwise                     = putStrLn "I don't understand!"
  where w1 = map toLower word1
        w2 = map toLower word2


-- Pattern in IS-questions: (is:name:in:the:location:?)
-- answerIs :: Foldable t => [String] -> t String -> Maybe Bool
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
-- Pattern in WHERE-questions (where:is:the:item:?)
answerWhereIs (_:_:_:item:_) story = 
  whereIsPerson (foldl (\person statement -> 
                       let s = words $ map toLower statement
                           item' = map toLower item
                       in  if item' `elem` s
                           then Just (head $ words statement) -- Name of the person posessing the item
                           else person
                ) Nothing story) story
answerWhere _ _ = "don't know"


-- Checks where is a person with a given name
-- TODO: TYPE SIGNATURE
whereIsPerson Nothing _ = "don't know"
whereIsPerson (Just name) story = 
  foldl (\loc statement -> 
        let (word1:word2:_) = words $ map toLower statement 
            name' = map toLower name
        in  if name' == word1 && word2 `elem` ["moved","went","journeyed"]
            then last $ words statement
            else loc
  ) "don't know" story


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
