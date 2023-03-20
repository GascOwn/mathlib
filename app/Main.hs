module Main (main) where

import Linear
import Data.Maybe (fromMaybe)

main :: IO ()
main = print $ fromMaybe
  "Cannot build an equation from an empty list"
  $ printRegressionLine [(59, 62), (63, 67), (67, 68), (71, 70), (75, 73)]

