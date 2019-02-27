
import System.Environment
import System.IO

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


-- Pattern in IS-questions: (Is:Name:In:The:Location:?)
answerIs :: Foldable t => [String] -> t String -> Maybe Bool
answerIs (_:name:_:_:location:_) story = 
  foldl (\acc statement -> let s = (words statement) in
                           if name `elem` s
                           then if location `elem` s && not ("no" `elem` s) && not ("either" `elem` s)
                                then Just True -- location found without negation or either statement
                                else if ("no" `elem` s && location `elem` s) || (not ("no" `elem` s) && not ("either" `elem` s))
                                     then Just False -- not in this particular location OR in some other location
                                     else if "no" `elem` s && not (location `elem` s)
                                          then Nothing -- No longer in some other location, so can possibly be in this location.
                                          else if "either" `elem` s && location `elem` s
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
prettyPrintList []     = do putStrLn "> End of story.\n"
prettyPrintList (x:xs) = do putStrLn x
                            prettyPrintList xs
