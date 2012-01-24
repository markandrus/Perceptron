module Perceptron (State(..), flattenState, lp, lp', kp, kp', lp2, lp2', gp, gp') where

import qualified Data.MemoCombinators as M
import qualified Data.Vector as V
import Data.List (inits)

mVectorInt = M.wrap (V.fromList) (V.toList) (M.list M.integral)
mVectorInt2 = M.memo2 (mVectorInt) (mVectorInt)

-- |State holds a learned or initialized weight vector and a list of prediction/truth tuples
-- It is helpful to thread State through our learning algorithms so that we can see how our
-- hypotheses form, weight vector changes, etc.
data State = State [V.Vector Int] [(Int, Int)]

-- |nextState updates a state with a new weight vector and prediction/truth tuple
-- Takes three arguments: State, a weight vector, and a prediction/truth tuple
-- Returns State
nextState :: State -> V.Vector Int -> (Int, Int) -> State
nextState (State ws pts) w pt = State (w:ws) (pt:pts)

-- |flattenState throws away our State's history for simplicity
flattenState :: State -> State
flattenState (State (w:_) (pt:_)) = State [w] [pt]

-- |Vector of zeros
-- It takes one argument for the vector dimension, of type 'Int'
zeros :: Int -> V.Vector Int
zeros = flip V.replicate 0

-- |Dot two vectors
dot :: Num a => V.Vector a -> V.Vector a -> a
dot ws vs = V.sum $ V.zipWith (*) ws vs

{- Linear perceptron implementation -}

-- |Linear perceptron
-- Takes two arguments: State and a list of vector/classification tuples
-- Returns State
lp' :: State -> [(V.Vector Int, Int)] -> State
lp' = foldl (\s@(State (w:_) _) (x, y) ->
  let y' = fromEnum (w `dot` x >= 0) in
    if y'==0 && y==1 then nextState s (V.zipWith (+) w x) (y,y')
    else if y'==1 && y==0 then nextState s (V.zipWith (-) w x) (y,y')
    else nextState s w (y,y'))

-- |Linear perceptron (starting from initial State)
-- Takes one argument: a list of vector/classification tuples
lp :: [(V.Vector Int, Int)] -> State
lp xys = lp' (State [zeros . V.length . fst $ head xys] []) xys

{- Kernel perceptron implementation -}

-- |Kernel perceptron
-- Takes three arguments: a kernel, State, and a list of vector/classification tuples
kp' :: (V.Vector Int -> V.Vector Int -> Int) -> State -> [(V.Vector Int, Int)] -> State
kp' k state xys = makeW . snd $ foldl (\(t, s@(State (c:_) _)) (x, y) ->
  let y' = fromEnum (f c x t >= 0) in
    if y'==0 && y==1 then (t+1, nextState s (c `V.snoc` 1) (y,y'))
    else if y'==1 && y==0 then (t+1, nextState s (c `V.snoc` (-1)) (y,y'))
    else (t+1, nextState s (c `V.snoc` 0) (y,y'))
  ) (0, state) xys where
  makeW (State (cs:_) pts) = State [foldl (V.zipWith (+)) cx cxs] pts where
    cx:cxs = map (\(c, x) -> V.map (\x' -> c * x') x) $ zip (V.toList cs) xs
  xs = fst $ unzip xys
  xss = V.map V.fromList . V.unsafeTail . V.fromList $ inits xs
  -- NOTE: The if statement below keeps us from computing x `k` xt when unnecessary. On our 2000-
  --       element training set, with the conditional we spend %40 less time computing k, and our
  --       total execution time is 3.5 times faster
  k' = mVectorInt2 k
  f c xt t = V.sum $ V.zipWith (\c x -> if c==0 then 0 else c * (x `k'` xt)) c (xss V.! t)

-- |Kernel perceptron  (starting from initial State)
-- Takes two arguments: a kernel and a list of vector/classification tuples
kp :: (V.Vector Int -> V.Vector Int -> Int) -> [(V.Vector Int, Int)] -> State
kp k = kp' k (State [V.empty] [])

{- Kernels -}

-- |Linear kernel
linear :: Num a => V.Vector a -> V.Vector a -> a
linear = dot

-- |Guassian kernel
-- Takes three arguments: sigma, and two Vectors
--gaussian :: Floating a => a -> V.Vector a -> V.Vector a -> Int
--gaussian sigma x x' = exp $ (eucNorm (V.zipWith (-) x x') ** 2) / (2 * (sigma ** 2)) where
--  eucNorm vs = sqrt . V.sum $ V.map (**2) vs -- Euclidean norm of a vector

{- Kernel perceptrons -}

-- |Linear perceptron in terms of the kernel perceptron
lp2' :: State -> [(V.Vector Int, Int)] -> State
lp2' = kp' linear

-- |Linear perceptron in terms of the kernel perceptron (starting from initial State)
lp2 :: [(V.Vector Int, Int)] -> State
lp2 = kp linear

-- |Guassian kernel perceptron
-- Takes three arguments: sigma, State, and a list of vector/classification tuples
--gp' :: Double -> State -> [(V.Vector Int, Int)] -> State
--gp' sigma = kp' (gaussian sigma)

-- |Guassian kernel perceptron (starting from initial State)
-- Takes two arguments: sigma and a list of vector/classification tuples
--gp :: Double -> [(V.Vector Int, Int)] -> State
--gp sigma = kp (gaussian sigma)

gp' = lp2'
gp = lp2
