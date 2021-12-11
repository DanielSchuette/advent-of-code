{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day4
    ( solutionPart1
    , solutionPart2
    ) where

import           Data.List         as L
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TI
import qualified Data.Text.Lazy.IO as TLI
import qualified Data.Text.Read    as TR
import           Formatting
import           Prelude           as P

type Board = [[Int]]

outputFormat :: Format r (Int -> r)
outputFormat = "The final, winning score is " % int % " points."

-- | We'll fail miserably if `length ts` isn't a multiple of 5.
makeBoards :: [Text] -> [Board]
makeBoards [] = []
makeBoards ts = f ts : makeBoards (drop 5 ts)
  where
    f ts = map (map parseInt . filter (/= "") . T.splitOn " ") $ take 5 ts

-- | `runGame` represents the main loop of the game, testing for a win condition
-- on every round and returning the winning score if possible. Instead of
-- folding, we take the (probably more intuitive) recursive route.
runGame :: [Board] -> [Int] -> Int -> Int
runGame boards draws lastDraw =
    case getWinningBoard boards of
        Nothing    -> runGame (markBoards boards curDraw) (tail draws) curDraw
        Just board -> calcWinningScore board lastDraw
  where
    curDraw = head draws

-- | Here, we run the game and filter for those boards that already won until
-- there is only one board left. That particular board is then used to
-- determine the final score (the "losing score").
runGameUntilLosing :: [Board] -> [Int] -> Int -> Int
runGameUntilLosing [] _ _ =
    error "AOC.Day4.runGameUntilLosing: ran out of boards to play with"
runGameUntilLosing b [] _ =
    error $
    "AOC.Day4.runGameUntilLosing: ran out of draws to mark on boards:\n" ++
    show b
runGameUntilLosing boards@[lastBoard] draws lastDraw =
    if isWinning lastBoard
        then calcWinningScore lastBoard lastDraw
        else runGameUntilLosing (markBoards boards curDraw) (tail draws) curDraw
  where
    curDraw = head draws
runGameUntilLosing boards draws lastDraw =
    case getWinningBoard boards of
        Nothing ->
            runGameUntilLosing (markBoards boards curDraw) (tail draws) curDraw
        -- There might be _more_ than one winning board per "round".
        Just winningBoard ->
            let boards' = filter (/= winningBoard) boards
             in runGameUntilLosing boards' draws lastDraw
  where
    curDraw = head draws

-- | Marking a board means replacing all occurrences of a certain integer with
-- `-1`. `getWinningBoard` relies on that convention, too.
markBoards :: [Board] -> Int -> [Board]
markBoards b m = map (markBoard m) b
  where
    markBoard m =
        map
            (map (\i ->
                      if i == m
                          then -1
                          else i))

getWinningBoard :: [Board] -> Maybe Board
getWinningBoard [] = Nothing
getWinningBoard (t:ts) =
    if isWinning t
        then Just t
        else getWinningBoard ts

isWinning :: Board -> Bool
isWinning b = rowWinning b || columnWinning b
  where
    rowWinning = any (all (== -1))
    columnWinning = rowWinning . transpose -- probably working?

calcWinningScore :: Board -> Int -> Int
calcWinningScore b m = (*) m $ sum $ filter (/= -1) $ concat b

parseInt :: Text -> Int
parseInt t = f $ TR.decimal t
  where
    f (Right (x, _)) = x :: Int
    f _ =
        error $
        "AOC.Day4.solutionPart1: unable to parse int from `" ++
        T.unpack t ++ "'"

getDraws :: [Text] -> [Int]
getDraws = map parseInt . T.splitOn "," . P.head

getBoards :: [Text] -> [Board]
getBoards = makeBoards . P.filter (\x -> x /= "\n" && x /= "") . P.tail

solutionPart1 :: IO ()
solutionPart1 = do
    dataPoints <- T.lines <$> TI.readFile "./data/input4"
    let drawnNums = getDraws dataPoints
    let boards = getBoards dataPoints
    let winningScore = runGame boards drawnNums (-1)
    TLI.putStrLn $ format outputFormat winningScore

solutionPart2 :: IO ()
solutionPart2 = do
    dataPoints <- T.lines <$> TI.readFile "./data/input4"
    let drawnNums = getDraws dataPoints
    let boards = getBoards dataPoints
    let losingScore = runGameUntilLosing boards drawnNums (-1)
    TLI.putStrLn $ format outputFormat losingScore
