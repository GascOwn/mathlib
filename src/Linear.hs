module Linear ( printRegressionLine
              , regressionLine
              , lineEquation
              , belongsLine
              , distance
              , Line) where

import Data.Tuple.Extra (both)
import Data.Decimal
import Data.Maybe (fromJust)
import Lib (Point)

data Line a = Line {
  m :: a,
  c :: a
}

average :: RealFrac a => [a] -> a
average xs = sum xs / fromIntegral (Prelude.length xs)

printRegressionLine :: RealFrac a => [Point a] -> Maybe String
printRegressionLine [] = Nothing
printRegressionLine xs = 
  let line = fromJust $ regressionLine xs
  in Just ("y = " ++ show (m line) ++ "x" ++ show (c line))

regressionLine :: RealFrac a => [Point a] -> Maybe (Line Decimal)
regressionLine [] = Nothing
regressionLine xs =
  let (xa, ya) = both average (unzip xs)
      m = sum ((\(x, y) -> (x - xa) * (y - ya)) <$> xs) /
          sum ((\(x, _) -> (x - xa) ^ (2 :: Int)) <$> xs)
      c = realFracToDecimal 3 (m * xa - ya)
  in Just Line {m = realFracToDecimal 3 m, c = c}

lineEquation :: RealFrac a => Point a -> Point a -> Line Decimal
lineEquation p q =
  let m = (snd q - snd p) / (fst q - fst p)
      c = realFracToDecimal 3  $ m * fst p - snd p
  in Line {m = realFracToDecimal 3 m, c = c}

belongsLine :: (Eq a, Num a) => Point a -> Line a -> Bool
belongsLine (x, y) line = y == m line * x + c line

distance :: Floating a => Point a -> Point a -> a
distance p q = sqrt $ (snd q - snd p) ^ (2 :: Int) + (fst q - fst p) ^ (2 :: Int)