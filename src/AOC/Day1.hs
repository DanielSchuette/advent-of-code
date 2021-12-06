{-# LANGUAGE OverloadedStrings #-}

module AOC.Day1
    ( solutionPart1
    , solutionPart2
    ) where

import           Data.List
import qualified Data.Text.Lazy.IO     as T
import           Formatting
import           Formatting.Formatters ()

outputFormat :: Format r (Int -> r)
outputFormat = "There are " % int % " increasing measurements."

countPairs' :: (a -> b -> Bool) -> [(a, b)] -> Int
countPairs' f =
    foldl'
        (\acc p ->
             if uncurry f p
                 then acc + 1
                 else acc)
        0

dataPoints :: IO [Int]
dataPoints = map read . lines <$> readFile "./data/input1" :: IO [Int]

solutionPart1 :: IO ()
solutionPart1 = do
    m <- dataPoints
    let numIncreasing = countPairs' (<) $ zip m (tail m)
    T.putStrLn $ format outputFormat numIncreasing

solutionPart2 :: IO ()
solutionPart2 = do
    m <- dataPoints
    let w = map tripleSum $ zip3 m (tail m) (tail $ tail m)
    let numIncreasing = countPairs' (<) $ zip w (tail w)
    T.putStrLn $ format outputFormat numIncreasing
  where
    tripleSum (x, y, z) = x + y + z
