module Main where
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import System.Environment
import System.Process

tapeLength :: Int
tapeLength = 2^15

data Vocab = MoveL | MoveR | Inc | Dec | Out | In | LoopStart | LoopEnd 
  deriving (Eq, Show)

type Memory = [Int]
type DataPointer = Int
type ProgramPointer = Int
type LoopTracker = (M.Map Int Int, M.Map Int Int)
type Instructions = [Vocab]
type Program = (Instructions, LoopTracker)
type System = (DataPointer, ProgramPointer, Memory)

initSystem :: System
initSystem = (0,0,initMemory)
  where initMemory =  take tapeLength [0,0..]

isFinished :: System -> Program -> Bool
isFinished (_, pos, _) (prog, _)
  | pos == length prog = True
  | otherwise = False

loopLevelCount :: [Vocab] -> [(Int, Int)]
loopLevelCount prog = zip [0..] loopLevels
  where f x LoopStart = x + 1
        f x LoopEnd = x - 1
        f x _ = x
        loopLevels = tail $ scanl f 0 prog

findLoopEnd :: Int -> [(Int, Int)] -> Int
findLoopEnd i loopLevels = minimum valid
  where valid = [fst x | x <- loopLevels, fst x > i, snd x == target]
        target = snd (loopLevels !! i) - 1

progToLoopTracker :: Instructions -> LoopTracker
progToLoopTracker prog = (loopStartMap, loopEndMap)
  where loopLevels = loopLevelCount prog
        loopStartInds = elemIndices LoopStart prog
        loopEndInds = (`findLoopEnd` loopLevels) <$> loopStartInds
        loopStartMap = M.fromList $ zip loopStartInds loopEndInds
        loopEndMap = M.fromList $ zip loopEndInds loopStartInds

applyAtIndex :: Memory -> (Int -> Int) -> Int -> Memory
applyAtIndex mem f i = base ++ newVal:end
  where (base, oldVal:end) = splitAt i mem
        newVal = f oldVal

changeAt :: Int -> Memory -> Int -> Memory
changeAt i mem newVal = base ++ newVal:end
  where (base, _:end) = splitAt i mem

--The otherwise should never be hit, but why break a good habit?
step :: Program -> System -> IO System
step (prog, loopTracker) (dataPos, progPos, memory)
  | symbol == MoveL = return (dataPos - 1, progPos + 1, memory)
  | symbol == MoveR = return (dataPos + 1, progPos + 1, memory)
  | symbol == Inc = return (dataPos, progPos + 1, inc memory)
  | symbol == Dec = return (dataPos, progPos + 1, dec memory)
  | symbol == Out = printState
  | symbol == In = getInput
  | symbol == LoopStart = return checkLoopStart
  | symbol == LoopEnd = return checkLoopEnd
  | otherwise = return (dataPos, progPos + 1, memory)
  where symbol = prog !! progPos
        inc x = applyAtIndex x (+1) dataPos
        dec x = applyAtIndex x (+(-1)) dataPos
        printState = do
          putChar . chr $ memory !! dataPos
          return (dataPos, progPos + 1, memory)
        getInput = do
          putStrLn "\nInput value:"
          char <- ord <$> getChar
          let newMemory = changeAt dataPos memory char
          return (dataPos, progPos + 1, newMemory)
        checkLoopStart
          | memory !! dataPos == 0 = (dataPos, loopStartToEnd M.! progPos + 1, memory)
          | otherwise = (dataPos, progPos + 1, memory)
        checkLoopEnd
          | memory !! dataPos /= 0 = (dataPos, loopEndToStart M.! progPos + 1, memory)
          | otherwise = (dataPos, progPos+1, memory)
        loopStartToEnd = fst loopTracker
        loopEndToStart = snd loopTracker

interpret :: System -> Program -> IO ()
interpret sys prog = do
  newSystem <- step prog sys
  if isFinished newSystem prog
    then return ()
    else interpret newSystem prog


intToVocab :: Int -> Vocab
intToVocab i 
  | i == 0 = MoveL
  | i == 1 = MoveR
  | i == 2 = Inc
  | i == 3 = Dec
  | i == 4 = Out
  | i == 5 = In
  | i == 6 = LoopStart
  | i == 7 = LoopEnd 
  | otherwise = MoveR

logToProg :: String -> Program
logToProg gitOutput = (prog, loopLevels)
  where removeSpaces x = [char | char <- x, char /= ' ']
        commits = length . removeSpaces <$> reverse (lines gitOutput)
        prog = intToVocab . (`mod` 8) <$> commits
        loopLevels = progToLoopTracker prog

authorToProg :: String -> IO Program
authorToProg author = logToProg <$> stdout
  where stdout = readProcess "git" args []
        args = ["log", "--pretty=%s", "--author=" ++ author]

main :: IO ()
main = do
  args <- getArgs
  let authorName = last args
  authorToProg authorName >>= interpret initSystem
