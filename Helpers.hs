module Helpers
( getStory
, prettyPrintStory
, maybe2string
, maybeStr2string
) where

import System.IO
import System.Environment

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
