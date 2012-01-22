import Char
import List
import Test.QuickCheck
import Text.Printf

import qualified Perceptron as P
import qualified Data.Vector as V

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList arbitrary

instance CoArbitrary a => CoArbitrary (V.Vector a) where
  coarbitrary = coarbitrary . V.toList

-- Running the linear perceptron is the same as running the kernel perceptron
prop_linearKernelLinear :: NonEmptyList (V.Vector Integer, Bool) -> Bool
prop_linearKernelLinear (NonEmpty exs) = w == w' && pts == pts' where
  (P.State (w:_) pts) = P.lp exs''
  (P.State (w':_) pts') = P.lp2 exs''
  -- I don't know QuickCheck well enough, so the following are hacks:
  --   1. We round the elements of our Vector Doubles so that the math is more reliable
  --   2. We pad our generated example vectors to the length of the largest vector
  exs'' = map (\(x,y) -> (x V.++ (V.replicate (n - V.length x) 0.0), y)) exs'
  n = foldl (\l (x,y) -> if V.length x > l then V.length x else l) 0 exs'
  exs' = map (\(x,y) -> (V.map (\x -> fromIntegral x) x, fromEnum y)) exs

tests = [ ("linear/kernel.linear", quickCheck prop_linearKernelLinear) ]
