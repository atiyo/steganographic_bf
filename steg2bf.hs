module Blah where
import System.Environment
import System.Process

intToChar :: Int -> Char
intToChar i 
  | i == 0 = '<'
  | i == 1 = '>'
  | i == 2 = '+'
  | i == 3 = '-'
  | i == 4 = '.'
  | i == 5 = ','
  | i == 6 = '['
  | i == 7 = ']' 

logToProg :: String -> String
logToProg log = intToChar . (`mod` 8) <$> commits
  where removeSpaces x = [char | char <- x, char /= ' ']
        commits = length . removeSpaces <$> reverse (lines log)

authorToProg :: String -> IO String
authorToProg author = logToProg <$> stdout
  where stdout = readProcess "git" args []
        args = ["log", "--pretty=%s", "--author=" ++ author]

main :: IO ()
main = do
  args <- getArgs
  let author = head args
  authorToProg author >>= putStrLn
