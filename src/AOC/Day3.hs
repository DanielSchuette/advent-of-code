{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day3
    ( solutionPart1
    , solutionPart2
    ) where

import           Data.ByteString as BS
import           Data.Text       as T
import qualified Data.Text.IO    as TI
import           Prelude         as P

readIntoBytes :: [Text] -> [ByteString]
readIntoBytes = P.map textToBS
  where
    textToBS t = undefined

solutionPart1 :: IO ()
solutionPart1 = do
    _ <- readIntoBytes . T.lines <$> TI.readFile "./data/input3"
    return ()

solutionPart2 :: IO ()
solutionPart2 = undefined
