{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day4
    ( solutionPart1
    , solutionPart2
    ) where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TI
import qualified Data.Text.Lazy.IO as TLI
import qualified Data.Text.Read    as TR
import           Formatting
import           Prelude           as P

type Board = [[Int]]

outputFormat :: Format r (Int -> r)
outputFormat = "The final, winning score is " % int % " points."

-- |
-- We'll fail miserably if `length ts` isn't a multiple of 5.
makeBoards :: [T.Text] -> [Board]
makeBoards [] = []
makeBoards ts = f ts : makeBoards (drop 5 ts)
  where
    f _ = [[1, 2, 3]] -- take 5 ts

solutionPart1 :: IO ()
solutionPart1 = do
    dataPoints <- T.lines <$> TI.readFile "./data/input4"
    let drawnNums = map (f . TR.decimal) $ T.splitOn "," $ P.head dataPoints
    let boards =
            makeBoards $
            P.filter (\x -> x /= "\n" && x /= "") $ P.tail dataPoints
    print boards
    TLI.putStrLn $ format outputFormat (-3)
  where
    f (Right (x, _)) = x :: Int
    f _              = error "AOC.Day4.solutionPart1: unable to parse draws"

solutionPart2 :: IO ()
solutionPart2 = do
    _ <- TI.readFile "./data/input4"
    TLI.putStrLn $ format outputFormat 12
