{-# LANGUAGE GADTs#-}

module Linear (printRegressionLine, regressionLine, lineEquation) where

import Data.Tuple.Extra (both)
import Data.Decimal
import Data.Maybe (fromJust)

type Point a = (a,a)

data Line a where Line :: Num a => {
  m :: a,
  c :: a
} -> Line a

average :: RealFrac a => [a] -> a
average xs = (/) (sum xs) (fromIntegral $ Prelude.length xs)

printRegressionLine :: RealFrac a => [Point a] -> Maybe String
printRegressionLine [] = Nothing
printRegressionLine xs = 
  let line = fromJust $ regressionLine xs
  in Just ("y = " ++ show (m line) ++ "x" ++ show (c line))

regressionLine :: RealFrac a => [Point a] -> Maybe (Line Decimal)
regressionLine [] = Nothing
regressionLine xs =
  let (xa, ya) = both average (unzip xs)
      m = (/)
        (sum $ (\(x, y) -> (x - xa) * (y - ya)) <$> xs)
        (sum $ (\(x, _) -> (x - xa) ^ (2 :: Int)) <$> xs)
      c = realFracToDecimal 3 (m * xa - ya)
  in Just Line {m = realFracToDecimal 3 m, c = c}

lineEquation :: RealFrac a => Point a -> Point a -> Line Decimal
lineEquation p q =
  let m = (snd q - snd p) / (fst q - fst p)
      c = realFracToDecimal 3  $ m * fst p - snd p
  in Line {m = realFracToDecimal 3 m, c = c}