module Perceptron
  ( normalize, State(..), flattenState, lpredict, lp, lp', kpredict, kp,
    kp', lpredict2, lp2, lp2', gpredict, gp, gp', makeW
  ) where

import qualified Data.Vector as V
import Data.List (inits)

-- |Calculate the L2-Norm of a vector
norm :: Floating c => V.Vector c -> c
norm vs = sqrt . V.sum $ V.map (**2) vs

-- |Normalize a vector
normalize :: Floating b => V.Vector b -> V.Vector b
normalize vs = V.map (/m) vs where m = norm vs

-- |State holds a learned or initialized weight vector and a list of prediction/truth tuples
-- It is helpful to thread State through our learning algorithms so that we can see how our
-- hypotheses form, weight vector changes, etc.
data State = State [V.Vector Double] [(Int, Int)]

-- |nextState updates a state with a new weight vector and prediction/truth tuple
-- Takes three arguments: State, a weight vector, and a prediction/truth tuple
-- Returns State
nextState :: State -> V.Vector Double -> (Int, Int) -> State
nextState (State ws pts) w pt = State (w:ws) (pt:pts)

-- |flattenState throws away our State's history for simplicity
flattenState :: State -> State
flattenState (State (w:_) (pt:_)) = State [w] [pt]

-- |Vector of zeros
-- It takes one argument for the vector dimension, of type 'Int'
zeros :: Int -> V.Vector Double
zeros = flip V.replicate 0

-- |Dot two vectors
dot :: Num a => V.Vector a -> V.Vector a -> a
dot ws vs = V.sum $ V.zipWith (*) ws vs

{- Linear perceptron implementation -}

-- |Predict given examples, xs, and weights, cs
lpredict :: State -> t -> V.Vector Double -> Bool
lpredict (State (ws:_) _) _ xt = 0 <= ws `dot` xt

-- |Linear perceptron
-- Takes two arguments: State and a list of vector/classification tuples
-- Returns State
lp' :: State -> [(V.Vector Double, Int)] -> State
lp' = foldl (\s@(State (ws:_) _) (xs,y) ->
  let y' = fromEnum (ws `dot` xs >= 0) in
    if y'==0 && y==1 then nextState s (V.zipWith (+) ws xs) (y,y')
    else if y'==1 && y==0 then nextState s (V.zipWith (-) ws xs) (y,y')
    else nextState s ws (y,y'))

-- |Linear perceptron (starting from initial State)
-- Takes one argument: a list of vector/classification tuples
lp :: [(V.Vector Double, Int)] -> State
lp xys = lp' (State [zeros . V.length . fst $ head xys] []) xys

{- Kernel perceptron implementation -}

-- |Predict with kernel, k, given State with weights cs, examples, xs, vector, xt
kpredict :: (V.Vector Double -> V.Vector Double -> Double) -> State -> [V.Vector Double] ->
            V.Vector Double -> Bool
kpredict k (State (cs:_) pts) xs xt =
  0 <= (V.sum $ V.zipWith (\c x -> if c==0 then 0 else c * (x `k` xt)) cs $ V.fromList xs)

-- |Kernel perceptron
-- Takes three arguments: a kernel, State, and a list of vector/classification tuples
kp' :: (V.Vector Double -> V.Vector Double -> Double) -> State -> [(V.Vector Double, Int)] -> State
-- kp' k state xys = makeW . snd $ foldl (\(t, s@(State (cs:_) _)) (xs,y) ->
kp' k state xys = snd $ foldl (\(t, s@(State (cs:_) _)) (xs,y) ->
  let y' = fromEnum (f cs xs t >= 0) in
    if y'==0 && y==1 then (t+1, nextState s (cs `V.snoc` 1) (y,y'))
    else if y'==1 && y==0 then (t+1, nextState s (cs `V.snoc` (-1)) (y,y'))
    else (t+1, nextState s (cs `V.snoc` 0) (y,y'))
  ) (0, state) xys where
  -- NOTE: The if statement below keeps us from computing x `k` xt when unnecessary. On our 2000-
  --       element training set, with the conditional we spend %40 less time computing k, and our
  --       total execution time is 3.5 times faster
  f cs xt t = V.sum $ V.zipWith (\c x -> if c==0 then 0 else c * (x `k` xt)) cs (xss V.! t)
  xs = fst $ unzip xys
  xss = V.map V.fromList . V.tail . V.fromList $ inits xs

-- |Kernel perceptron  (starting from initial State)
-- Takes two arguments: a kernel and a list of vector/classification tuples
kp :: (V.Vector Double -> V.Vector Double -> Double) -> [(V.Vector Double, Int)] -> State
kp k = kp' k (State [V.empty] [])

{- Kernels -}

-- |Linear kernel
linear :: Num a => V.Vector a -> V.Vector a -> a
linear = dot

-- |Guassian kernel
-- Takes three arguments: sigma, and two Vectors
gaussian :: Floating a => a -> V.Vector a -> V.Vector a -> a
gaussian sigma x x' = exp $ (norm (V.zipWith (-) x x') ** 2) / (2 * (sigma ** 2)) where
  eucNorm vs = sqrt . V.sum $ V.map (**2) vs -- Euclidean norm of a vector

{- Kernel perceptrons -}

-- |Predict with linear kernel
lpredict2 :: State -> [V.Vector Double] -> V.Vector Double -> Bool
lpredict2 = kpredict linear

-- |makeW lets you flatten the cs from lp2 to ws (as in lp)
makeW :: State -> [V.Vector Double] -> State
makeW (State (cs:_) pts) xs = State [foldl (V.zipWith (+)) cx cxs] pts where
  -- NOTE: Calculating zeroV once rather than zeroing a vector decreases execution time for kp'
  --       by 0.7%, yielding ~200 ms decrease in execution time for our 2000-element training set
  cx:cxs = map (\(c,x) -> if c==0 then zeroV else V.map (\x' -> c * x') x) $ zip (V.toList cs) xs
  zeroV = zeros . V.length $ head xs

-- |Linear perceptron in terms of the kernel perceptron
lp2' :: State -> [(V.Vector Double, Int)] -> State
lp2' = kp' linear

-- |Linear perceptron in terms of the kernel perceptron (starting from initial State)
lp2 :: [(V.Vector Double, Int)] -> State
lp2 = kp linear

-- |Predict with Gaussian kernel, given sigma
gpredict :: Double -> State -> [V.Vector Double] -> V.Vector Double -> Bool
gpredict sigma = kpredict (gaussian sigma)

-- |Guassian kernel perceptron
-- Takes three arguments: sigma, State, and a list of vector/classification tuples
gp' :: Double -> State -> [(V.Vector Double, Int)] -> State
gp' sigma = kp' (gaussian sigma)

-- |Guassian kernel perceptron (starting from initial State)
-- Takes two arguments: sigma and a list of vector/classification tuples
gp :: Double -> [(V.Vector Double, Int)] -> State
gp sigma = kp (gaussian sigma)
