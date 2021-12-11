{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day6
    ( solutionPart1
    , solutionPart2
    ) where

import           Data.Either       (fromRight)
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TI
import qualified Data.Text.Lazy.IO as TLI
import qualified Data.Text.Read    as TR
import           Formatting
import           Prelude           as P

-- | We format our solution as follows:
outputFormat :: Format r (Int -> r)
outputFormat = "After 80 days, there are " % int % " little lanternfish."

-- | We return a potentially dangerous value `0` if parsing fails.
parseInt :: Text -> Int
parseInt = fst . fromRight (0, undefined) . TR.decimal

solutionPart1 :: IO ()
solutionPart1 = do
    dataPoints <- T.lines <$> TI.readFile "./data/input6"
    let result = 0
    TLI.putStrLn $ format outputFormat result

solutionPart2 :: IO ()
solutionPart2 = do
    dataPoints <- T.lines <$> TI.readFile "./data/input6"
    let result = 0
    TLI.putStrLn $ format outputFormat result
