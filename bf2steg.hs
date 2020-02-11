module Main where
import Data.List
import System.Environment

data Vocab = MoveL | MoveR | Inc | Dec | Out | In | LoopStart | LoopEnd 
  deriving (Eq, Show)
type Program = [Vocab]

bfToVocab :: Char -> [Vocab]
bfToVocab c
  | c == '<' = [MoveL]
  | c == '>' = [MoveR]
  | c == '+' = [Inc]
  | c == '-' = [Dec]
  | c == '.' = [Out]
  | c == ',' = [In]
  | c == '[' = [LoopStart]
  | c == ']' = [LoopEnd]
  | otherwise = []

bfToProg :: String -> Program
bfToProg s = foldl mappend [] vocabList
  where vocabList = bfToVocab <$> s

vocabToString :: Vocab -> String
vocabToString v 
  | v == MoveL = "Oh yes sir"
  | v == MoveR = "Oh yes sir."
  | v == Inc = "I can boogie"
  | v == Dec = "I can boogie."
  | v == Out = "But I need a certain song"
  | v == In = "But I need a certain song."
  | v == LoopStart = "But I will give you one more chance.."
  | v == LoopEnd = "You wanna know if I can dance"

prepCommit :: String -> String -> String
prepCommit author i = "git commit --allow-empty " ++ messageString ++ authorString
  where messageString = "-m '" ++ i ++ "' "
        authorString = "--author='" ++ author ++ " <a@a>'"

bfToCommits :: String -> String -> String
bfToCommits author bf = intercalate "\n" commits
  where commits = prepCommit author . vocabToString <$> bfToProg bf

main :: IO ()
main = do
  args <- getArgs
  let author = args !! 1
  bf <- readFile $ head args
  writeFile (author ++ ".sh") $ bfToCommits author bf
