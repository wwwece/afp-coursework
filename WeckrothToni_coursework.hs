
import System.Environment
import System.IO

main :: IO ()
main = do 
  story <- getStory
  putStrLn "\n> THE STORY:"
  prettyPrintList story
  askQuestions story


-- TODO: TYPE SIGNATURE
askQuestions story = do
  putStr "ASK A QUESTION: "
  question <- getLine
  putStr $ question ++ " "
  answer (words question) story
  askQuestions story


-- TODO: TYPE SIGNATURE
answer question@(q1:q2:qs) story
  | q1 == "Is"                    = putStrLn $ case (answerIs question story) of 
                                                    Just True  -> "yes"
                                                    Just False -> "no"
                                                    Nothing    -> "maybe"
  | q1 == "Where" && q2 == "is"   = putStrLn $ q1 ++ q2
  | q1 == "Where" && q2 == "was"  = putStrLn $ q1 ++ q2
  | q1 == "How"   && q2 == "many" = putStrLn $ q1 ++ q2
  | q1 == "How"   && q2 == "do"   = putStrLn $ q1 ++ q2
  | otherwise                     = putStrLn "maybe"


-- TODO: TYPE SIGNATURE
answerIs (_:who:_:_:location:_) story = 
  foldl (\acc s -> if who `elem` (words s) && location `elem` (words s) 
                   then Just True 
                   else if who `elem` (words s) 
                        then Just False 
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
prettyPrintList []     = do putStrLn "> End of story.\n"
prettyPrintList (x:xs) = do putStrLn x
                            prettyPrintList xs
