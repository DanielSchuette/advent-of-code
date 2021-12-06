{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day3
    ( solutionPart1
    , solutionPart2
    ) where

import           Data.List             as L hiding (words)
import           Data.Text             as T hiding (words)
import qualified Data.Text.IO          as TI
import qualified Data.Text.Lazy.IO     as TLI
import           Formatting
import           Prelude               as P hiding (words)

-- |
-- This function is highly unsafe. If `n` cannot 0-index into `ts`, then we
-- crash with an exception.
mostFrequent :: [Text] -> Int -> Char
mostFrequent ts n
    | isMostFrequent ts n '1' = '1'
    | isMostFrequent ts n '0' = '0'
    | otherwise = error "AOC.Day3.mostFrequent: got chars other than `0' or `1'"

-- |
-- `isMostFrequent` tests whether char `c` occurs _at least_ as often as all
-- other chars in column `n` of the input `ts`. Unsafe in a sense that an
-- invalid column index will crash as well as malformed strings.
-- It is important to note that it is absolutely possible that both `0` and `1`
-- seem to be most frequent, since we also count characters that are at least
-- as frequent as any other char in the input (e.g. 4 out of 8 chars being the
-- same).
isMostFrequent :: [Text] -> Int -> Char -> Bool
isMostFrequent ts n c
    | n >= T.length (P.head ts) = error $ show ts
    | otherwise =
        let columnSum =
                L.foldl'
                    (\acc t ->
                         if index t n == c
                             then acc + 1
                             else acc)
                    0
                    ts
         in 2 * columnSum >= P.length ts

-- |
-- `findOxygenRating` and `findCO2Rating` are highly redundant. Currently, I
-- don't see a way to pull out the logic that differentiates them.
findOxygenRating :: Int -> Int -> [Text] -> Int
findOxygenRating n m ts
    | P.length ts == 1 = bitsToNum $ P.head ts
    | n >= m = error "AOC.Day3.findOxygenRating: too many strings left"
    | mc1 = findOxygenRating (n + 1) m $ P.filter (\t -> index t n == '1') ts
    | mc0 = findOxygenRating (n + 1) m $ P.filter (\t -> index t n == '0') ts
    | otherwise = error "AOC.Day3.findOxygenRating: unexpected error"
  where
    mc0 = isMostFrequent ts n '0'
    mc1 = isMostFrequent ts n '1'

findCO2Rating :: Int -> Int -> [Text] -> Int
findCO2Rating n m ts
    | P.length ts == 1 = bitsToNum $ P.head ts
    | n >= m = error "AOC.Day3.findOxygenRating: too many strings left"
    | mc1 && mc0 =
        findCO2Rating (n + 1) m $ P.filter (\t -> index t n == '0') ts
    | mc0 = findCO2Rating (n + 1) m $ P.filter (\t -> index t n == '1') ts
    | mc1 = findCO2Rating (n + 1) m $ P.filter (\t -> index t n == '0') ts
    | otherwise = error "AOC.Day3.findOxygenRating: unexpected error"
  where
    mc0 = isMostFrequent ts n '0'
    mc1 = isMostFrequent ts n '1'

flipBit :: Char -> Char
flipBit c
    | c == '1' = '0'
    | c == '0' = '1'
    | otherwise = error "AOC.Day3.flipBit: invalid input"

-- |
-- "1000101" counterintuitively assumes the most significant bit to be to the
-- right, so we can just fold up from the left.
bitsToNum :: Text -> Int
bitsToNum =
    T.foldl'
        (\acc c ->
             if c == '1'
                 then 2 * acc + 1
                 else 2 * acc)
        0

getWords :: IO [Text]
getWords = T.lines <$> TI.readFile "./data/input3"

bitsPerWord :: [Text] -> Int
bitsPerWord w = T.length $ P.head w

outputFormat :: Format r (Int -> r)
outputFormat = "The (multiplied) rating is " % int % " imaginary units :)"

solutionPart1 :: IO ()
solutionPart1 = do
    words <- getWords
    let bpw = bitsPerWord words
    let moreFrequentBits =
            T.reverse $ pack $ P.map (mostFrequent words) [0 .. bpw - 1]
    let lessFrequentBits = T.map flipBit moreFrequentBits
    let moreFrequentNum = bitsToNum moreFrequentBits
    let lessFrequentNum = bitsToNum lessFrequentBits
    TLI.putStrLn $ format outputFormat $ moreFrequentNum * lessFrequentNum

solutionPart2 :: IO ()
solutionPart2 = do
    words <- getWords
    let bpw = bitsPerWord words
    let oxygen = findOxygenRating 0 bpw words
    let co2 = findCO2Rating 0 bpw words
    TLI.putStrLn $ format outputFormat $ oxygen * co2
