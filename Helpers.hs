module Helpers
( getStory
, prettyPrintStory
, maybe2string
, maybeStr2string
) where

import System.IO
import System.Environment

-- Gets story from the file which name is 
-- given as a first command line argument
getStory :: IO [String]
getStory = do
  args <- getArgs
  file <- readFile $ head args
  return $ lines file

-- "Pretty prints" the story (i.e. list of strings)
prettyPrintStory :: [String] -> IO ()
prettyPrintStory xs = do putStrLn "\n> THE STORY:"; prettyPrintStory' xs
  where prettyPrintStory' []     = do putStrLn "> END OF STORY.\n"
        prettyPrintStory' (x:xs) = do putStrLn ("  " ++ x)
                                      prettyPrintStory' xs

-- Translates Maybe value into String
maybe2string :: Show a => Maybe a -> String
maybe2string Nothing = "don't know"
maybe2string (Just x) = show x

-- Translates Maybe String value into String
maybeStr2string :: Maybe String -> String
maybeStr2string Nothing = "don't know"
maybeStr2string (Just x) = x
