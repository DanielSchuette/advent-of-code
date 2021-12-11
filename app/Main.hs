module Main where

import qualified AOC.Day1 as D1 (solutionPart1, solutionPart2)
import qualified AOC.Day2 as D2 (solutionPart1, solutionPart2)
import qualified AOC.Day3 as D3 (solutionPart1, solutionPart2)
import qualified AOC.Day4 as D4 (solutionPart1, solutionPart2)
import qualified AOC.Day5 as D5 (solutionPart1, solutionPart2)
import qualified AOC.Day6 as D6 (solutionPart1, solutionPart2)

solutions :: [IO ()]
solutions =
    [ D1.solutionPart1
    , D1.solutionPart2
    , D2.solutionPart1
    , D2.solutionPart2
    , D3.solutionPart1
    , D3.solutionPart2
    , D4.solutionPart1
    , D4.solutionPart2
    , D5.solutionPart1
    , D5.solutionPart2
    , D6.solutionPart1
    , D6.solutionPart2
    ]

main :: IO ()
main = sequence_ solutions
