import Data.Char
import Data.List
import Test.QuickCheck
import Text.Printf

import Control.Arrow

import qualified Perceptron as P
import qualified Data.Vector as V
import Misc

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList arbitrary

instance CoArbitrary a => CoArbitrary (V.Vector a) where
  coarbitrary = coarbitrary . V.toList

prop_sortShuffleSort :: NonEmptyList (Integer) -> Bool
prop_sortShuffleSort (NonEmpty xs) = sort xs == (sort $ shuffle xs)

-- Running the linear perceptron is the same as running the (linear) kernel perceptron
prop_lpLp2 :: NonEmptyList (V.Vector Integer, Bool) -> Bool
prop_lpLp2 (NonEmpty exs) = w == w' && pts == pts' where
  (P.State (w:_) pts) = P.lp exs''
  (P.State (w':_) pts') = P.makeW (P.lp2 exs'') xs
  xs = fst $ unzip exs''
  -- I don't know QuickCheck well enough, so the following are hacks:
  --   1. We round the elements of our Vector Doubles so that the math is more reliable
  --   2. We pad our generated example vectors to the length of the largest vector
  exs'' = map (\(x,y) -> (x V.++ V.replicate (n - V.length x) 0.0, y)) exs'
  n = foldl (\l (x,y) -> if V.length x > l then V.length x else l) 0 exs'
  -- exs' = map (\(x,y) -> (V.map (fromIntegral) x, fromEnum y)) exs
  exs' = map (V.map fromIntegral Control.Arrow.*** fromEnum) exs

-- Predicting with the linear perceptron is the same as predicting with the (linear) kernel
-- perceptron
prop_lpredictLpredict2 :: NonEmptyList (V.Vector Integer, Bool) ->
                             NonEmptyList (V.Vector Integer) -> Bool
prop_lpredictLpredict2 (NonEmpty exs) (NonEmpty xts) = p == p' where
  p = map (\xt -> P.lpredict s [] xt) xts''
  p' = map (\xt -> P.lpredict2 s' xs xt) xts''
  s@(P.State (w:_) pts) = P.lp exs''
  s'@(P.State (w':_) pts') = P.lp2 exs''
  xs = fst $ unzip exs''
  -- Pad each xt to longest length
  xts'' = map (\x -> (x V.++ V.replicate (n - V.length x) 0.0)) xts'
  xts' = map (\xt -> V.map fromIntegral xt) xts
  -- I don't know QuickCheck well enough, so the following are hacks:
  --   1. We round the elements of our Vector Doubles so that the math is more reliable
  --   2. We pad our generated example vectors to the length of the largest vector
  exs'' = map (\(x,y) -> (x V.++ V.replicate (n - V.length x) 0.0, y)) exs'
  n = foldl (\l (x,y) -> if V.length x > l then V.length x else l) 0 exs'
  -- exs' = map (\(x,y) -> (V.map (fromIntegral) x, fromEnum y)) exs
  exs' = map (V.map fromIntegral Control.Arrow.*** fromEnum) exs

tests = [ ("sort.shuffle/sort", quickCheck prop_sortShuffleSort)
        , ("lp/lp2", quickCheck prop_lpLp2)
        , ("lpredict/lpredict2", quickCheck prop_lpredictLpredict2)
        ]
