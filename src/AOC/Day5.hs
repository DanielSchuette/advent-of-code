{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day5
    ( solutionPart1
    , solutionPart2
    ) where

import           Data.Either       (fromRight)
import           Data.List
import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as TI
import qualified Data.Text.Lazy.IO as TLI
import qualified Data.Text.Read    as TR
import           Formatting
import           Prelude           as P

type World = Map (Int, Int) Int

data Orientation
    = Horizontal
    | Vertical
    | Diagonal
    deriving (Show, Eq)

data Point =
    Point Int Int -- ^ The (x, y) coordinates in a Cartesian coordinate system.
    deriving (Show, Eq)

data LineSegment =
    LineSegment
        { start, end  :: Point
        , orientation :: Orientation -- ^ We need this field to filter segments.
        }
    deriving (Show, Eq)

outputFormat :: Format r (Int -> r)
outputFormat = "There are " % int % " dangerous tiles with vents we must avoid."

sign :: Int -> Int
sign x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1
    | otherwise = error "AOC.Day5.sign: unexpected input"

-- | We return a potentially dangerous value `0` if parsing fails.
parseInt :: Text -> Int
parseInt = fst . fromRight (0, undefined) . TR.decimal

parseInput :: [Text] -> [LineSegment]
parseInput = map parseSegmentInfo

parseSegmentInfo :: Text -> LineSegment
parseSegmentInfo inp =
    let coords = concatMap (map parseInt . T.splitOn ",") $ T.splitOn " -> " inp
     in if length coords /= 4 -- for good measure
            then error $
                 "AOC.Day5.parseSegmentInfo: irregular input " ++ T.unpack inp
            else let [x1, y1, x2, y2] = coords
                     orien = orientationFromCoords x1 y1 x2 y2
                  in LineSegment (Point x1 y1) (Point x2 y2) orien

orientationFromCoords :: Int -> Int -> Int -> Int -> Orientation
orientationFromCoords x1 y1 x2 y2
    | y1 == y2 = Horizontal
    | x1 == x2 = Vertical
    | otherwise = Diagonal -- @BUG: This might lead to unexpected behavior.

insertSegmentsIntoWorld :: World -> [LineSegment] -> World
insertSegmentsIntoWorld = foldl' insertSegmentIntoWorld

insertSegmentIntoWorld :: World -> LineSegment -> World
insertSegmentIntoWorld world seg =
    let Point x1 y1 = start seg
        Point x2 y2 = end seg
        (x1', x2') = (min x1 x2, max x1 x2)
        (y1', y2') = (min y1 y2, max y1 y2)
        dx = sign $ x2 - x1
        dy = sign $ y2 - y1
     in case orientation seg of
            Horizontal ->
                foldl' (\w x -> M.insertWith (+) (x, y1) 1 w) world [x1' .. x2']
            Vertical ->
                foldl' (\w y -> M.insertWith (+) (x1, y) 1 w) world [y1' .. y2']
            Diagonal ->
                foldl'
                    (\w d -> M.insertWith (+) (x1 + d * dx, y1 + d * dy) 1 w)
                    world
                    [0 .. (x2' - x1')]

-- | Get segments with a filter function.
getSegments :: (LineSegment -> Bool) -> IO [LineSegment]
getSegments filterFunc =
    filter filterFunc . parseInput . T.lines <$> TI.readFile "./data/input5"

calcResult :: [LineSegment] -> Int
calcResult =
    foldl'
        (\acc v ->
             if v > 1
                 then acc + 1
                 else acc)
        0 .
    insertSegmentsIntoWorld M.empty

solutionPart1 :: IO ()
solutionPart1 = do
    segments <- getSegments (\s -> orientation s /= Diagonal)
    let result = calcResult segments
    TLI.putStrLn $ format outputFormat result

solutionPart2 :: IO ()
solutionPart2 = do
    segments <- getSegments (const True)
    let result = calcResult segments
    TLI.putStrLn $ format outputFormat result
