
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
answer question@(w1:w2:words) story
  | w1' == "is"                     = putStrLn $ case (answerIs question story) of 
                                                      Just True  -> "yes"
                                                      Just False -> "no"
                                                      Nothing    -> "maybe"
  | w1' == "where" && w2' == "is"   = putStrLn $ w1 ++ w2
  | w1' == "where" && w2' == "was"  = putStrLn $ w1 ++ w2
  | w1' == "how"   && w2' == "many" = putStrLn $ w1 ++ w2
  | w1' == "how"   && w2' == "do"   = putStrLn $ w1 ++ w2
  | otherwise                     = putStrLn "I don't understand!"
  where w1' = map toLower w1
        w2' = map toLower w2


-- Pattern in IS-questions: (Is:Name:In:The:Location:?)
answerIs :: Foldable t => [String] -> t String -> Maybe Bool
answerIs (_:name:_:_:location:_) story = 
  foldl (\acc statement -> 
        let s     = words $ map toLower statement
            name' = map toLower name 
            loc   = map toLower location
        in if name' `elem` s
           then if loc `elem` s && not ("no" `elem` s) && not ("either" `elem` s)
                then Just True -- location found without negation or either statement
                else if ("no" `elem` s && loc `elem` s) || (not ("no" `elem` s) && not ("either" `elem` s))
                     then Just False -- not in this particular location OR in some other location
                     else if "no" `elem` s && not (loc `elem` s)
                          then Nothing -- No longer in some other location, so can possibly be in this location.
                          else if "either" `elem` s && loc `elem` s
                               then Nothing -- Is either in this location, or some other location
                               else acc
           else acc) Nothing story
answerIs _ _ = Nothing



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
