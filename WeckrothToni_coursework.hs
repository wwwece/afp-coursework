import System.IO
import System.Environment
import Data.Char (toLower)
import Helpers (getStory, prettyPrintStory)
import StatementFilters
import AnswerMachine

-- Run program in terminal:
--     runhaskell WeckrothToni_coursework.hs story.txt < input_questions.txt
-- (Leave "< input_questions.txt" out and you can type the question one by one).

-- Everything seems to work quite fine. Tasks 1-5, in my opinion, 
-- should be well covered, but with Task 6 I had some challenges. 
-- It's still passing the tests, though, and is "almost there", but 
-- there's some parts that left me slighly unhappy. These issues are 
-- commented/described inside the AnswerMachine.answerHowDoYouGo function.
-- 
-- Sometimes the program isn't that flexible/general so that some 
-- statements/questions has to be exactly in a certain form to work. 
-- This probably could have been done differently (in more general way) 
-- but I guess my approach is enough to cover the requirements for 
-- this assignment. At least all the tests are giving the correct answers!
-- 
-- Other than above small concerns, I feel happy with my work.


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
  | word1' == "is"                        = putStrLn $         answerIs question (filter1 story)
  | word1' == "where" && word2' == "is"   = putStrLn $    answerWhereIs question (filter2 story)
  | word1' == "where" && word2' == "was"  = putStrLn $   answerWhereWas question (filter1 story)
  | word1' == "how"   && word2' == "many" = putStrLn $    answerHowMany question (filter3 story)
  | word1' == "how"   && word2' == "do"   = putStrLn $ answerHowDoYouGo question (filter4 story)
  | otherwise                             = putStrLn "I don't understand!"
  where word1' = map toLower word1
        word2' = map toLower word2
        filter1 = filter onlyPersonLocationStatements
        filter2 = filter onlyPersonAndItemLocationStatements
        filter3 = filter onlyItemCountStatements
        filter4 = filter onlyDirectionStatements
