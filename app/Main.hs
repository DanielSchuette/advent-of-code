module Main where

import qualified AOC.Day1 as D1 (solutionPart1, solutionPart2)
import qualified AOC.Day2 as D2 (solutionPart1, solutionPart2)
import qualified AOC.Day3 as D3 (solutionPart1, solutionPart2)

solutions :: [IO ()]
solutions =
    [ D1.solutionPart1
    , D1.solutionPart2
    , D2.solutionPart1
    , D2.solutionPart2
    , D3.solutionPart1
    , D3.solutionPart2
    ]

main :: IO ()
main = sequence_ solutions
