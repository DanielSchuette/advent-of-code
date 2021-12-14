{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day7
    ( solutionPart1
    , solutionPart2
    ) where

import           Data.Either       (fromRight)
import           Data.List         (foldl')
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TI
import qualified Data.Text.Lazy.IO as TLI
import qualified Data.Text.Read    as TR
import           Formatting
import           Prelude           as P

type FuelFunc = Int -> Int -> Int

-- | We format our solution as follows:
outputFormat :: Format r (Int -> r)
outputFormat = "The crabs need " % int % " gallons of fuel."

-- | We return a potentially dangerous value `0` if parsing fails.
parseInt :: Text -> Int
parseInt = fst . fromRight (0, undefined) . TR.decimal

calcFuel :: FuelFunc -> Int -> [Int] -> Int
calcFuel fuelFunc pos = foldl' (\acc crab -> acc + fuelFunc crab pos) 0

solve :: FuelFunc -> [Int] -> Int
solve fuelFunc crabs =
    foldl'
        (\curr v ->
             let cost = calcFuel fuelFunc v crabs
              in if cost < curr
                     then cost
                     else curr)
        (maxBound :: Int)
        [min' .. max']
  where
    min' = minimum crabs
    max' = maximum crabs

-- | `absDiff` calculates the absolute difference (i.e. the distance) between
-- its arguments `a` and `b`.
absDiff :: Int -> Int -> Int
absDiff a b = abs (a - b)

-- | `sumTo` uses Gauß' Formula to sum from `0` up to `n`.
sumTo :: Int -> Int
sumTo n = n * (n + 1) `div` 2

linearFuel :: FuelFunc
linearFuel = absDiff

increasingFuel :: FuelFunc
increasingFuel crab = sumTo . absDiff crab

solution :: FuelFunc -> IO ()
solution fuelFunc = do
    crabs <-
        map parseInt . T.splitOn "," . head . T.lines <$>
        TI.readFile "./data/input7"
    TLI.putStrLn $ format outputFormat $ solve fuelFunc crabs

solutionPart1 :: IO ()
solutionPart1 = solution linearFuel

solutionPart2 :: IO ()
solutionPart2 = solution increasingFuel
