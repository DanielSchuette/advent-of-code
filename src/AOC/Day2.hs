{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Day2
    ( solutionPart1
    , solutionPart2
    ) where

import           Data.List
import           Data.Text             as T hiding (foldl', map)
import qualified Data.Text.IO          as TI
import qualified Data.Text.Lazy.IO     as TLI
import qualified Data.Text.Read        as TR
import           Formatting
import           Formatting.Formatters ()
import           Prelude               as P hiding (lines)

data Direction
    = Forward Int
    | Down Int
    | Up Int
    deriving (Show, Eq)

type Pathfinder = [Direction] -> (Int, Int)

data SubmarinePosition =
    SubmarinePosition
        { horizontal :: Int
        , vertical   :: Int
        , aim        :: Int
        }
    deriving (Show, Eq)

startPos :: SubmarinePosition
startPos = SubmarinePosition 0 0 0

readDirection :: Text -> Maybe Direction
readDirection t
    | P.length s /= 2 = Nothing
    | dir == "forward" = Just $ Forward n
    | dir == "down" = Just $ Down n
    | dir == "up" = Just $ Up n
    | otherwise = Nothing
  where
    s = splitOn " " t
    [dir, num] = s -- probably enough to protect against malformed input
    Right (n, _) = TR.decimal num -- also potentially dangerous!

normalize :: [Text] -> [Direction]
normalize = map f
  where
    f t =
        case readDirection t of
            Just d -> d
            Nothing ->
                error $
                unpack $ append "AOC.Day2: normalize: unexpected direction " t

driver1 :: Pathfinder
driver1 = foldl' f (0, 0)
  where
    f (h, v) d =
        case d of
            Forward h' -> (h + h', v)
            Up v'      -> (h, v - v')
            Down v'    -> (h, v + v')

driver2 :: Pathfinder
driver2 ds =
    let (SubmarinePosition h v _) = foldl' f startPos ds
     in (h, v)
  where
    f (SubmarinePosition h v a) d =
        case d of
            Forward h' -> SubmarinePosition (h + h') (v + a * h') a
            Up v'      -> SubmarinePosition h v (a - v')
            Down v'    -> SubmarinePosition h v (a + v')

solutionWith :: Pathfinder -> IO ()
solutionWith driver = do
    submarinePath <- normalize . T.lines <$> TI.readFile "./data/input2"
    let (h, v) = driver submarinePath
    TLI.putStrLn $ format solutionFormat $ h * v
  where
    solutionFormat :: Format r (Int -> r)
    solutionFormat = "The (multiplied) submarine path is " % int % "."

solutionPart1 :: IO ()
solutionPart1 = solutionWith driver1

solutionPart2 :: IO ()
solutionPart2 = solutionWith driver2
