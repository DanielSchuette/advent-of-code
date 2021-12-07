{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day5
    ( solutionPart1
    , solutionPart2
    ) where

import qualified Data.Text         as T
import qualified Data.Text.IO      as TI
import qualified Data.Text.Lazy.IO as TLI
import qualified Data.Text.Read    as TR
import           Formatting
import           Prelude           as P

outputFormat :: Format r (Int -> r)
outputFormat = "Placeholder result " % int % "."

solutionPart1 :: IO ()
solutionPart1 = do
    dataPoints <- T.lines <$> TI.readFile "./data/input4"
    let result = 0
    TLI.putStrLn $ format outputFormat result

solutionPart2 :: IO ()
solutionPart2 = do
    dataPoints <- T.lines <$> TI.readFile "./data/input4"
    let result = 0
    TLI.putStrLn $ format outputFormat result
