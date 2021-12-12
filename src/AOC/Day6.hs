{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day6
    ( solutionPart1
    , solutionPart2
    ) where

import           Data.Either       (fromRight)
import           Data.List         (foldl')
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TI
import qualified Data.Text.Lazy.IO as TLI
import qualified Data.Text.Read    as TR
import           Formatting
import           Prelude           as P

-- | `FishPopulation` maps fish ages in the range [0, 8] to a population size.
type FishPopulation = Map Integer Integer

-- | We format our solution as follows:
outputFormat :: Format r (Int -> Integer -> r)
outputFormat =
    "After " % int % " days, there are " % int % " little lanternfish."

-- | We return a potentially dangerous value `0` if parsing fails.
parseInt :: Text -> Integer
parseInt = fst . fromRight (0, undefined) . TR.decimal

createPopulation :: [Integer] -> FishPopulation
createPopulation = foldl' (\map age -> M.insertWith (+) age 1 map) M.empty

runSimulation :: Int -> FishPopulation -> FishPopulation
runSimulation 0 = id
runSimulation days = runSimulation (days - 1) . makeNewPop
  where
    makeNewPop =
        foldl'
            (\map (age, cnt) ->
                 if age > 0
                     then M.insertWith (+) (age - 1) cnt map
                     else M.insertWith (+) 6 cnt (M.insertWith (+) 8 cnt map))
            M.empty .
        M.toList

countPopulation :: FishPopulation -> Integer
countPopulation = foldl' (+) 0

solution :: Int -> IO ()
solution days = do
    initialPopulation <-
        createPopulation . map parseInt . T.splitOn "," . head . T.lines <$>
        TI.readFile "./data/input6"
    let result = countPopulation $ runSimulation days initialPopulation
    TLI.putStrLn $ format outputFormat days result

solutionPart1 :: IO ()
solutionPart1 = do
    solution 80

solutionPart2 :: IO ()
solutionPart2 = do
    solution 256
