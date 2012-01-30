module Main (main) where
 
import System
import System.IO
import System.IO.Error

import Control.Exception (bracket, bracket_)
import Control.Monad
import Control.Arrow

import Data.List

import qualified Data.Vector as V

import Options
import qualified Perceptron as P
import Misc

{- Main -}

showListAsCSV :: Show a => [a] -> String
showListAsCSV [x] = show x
showListAsCSV (x:xs) = show x ++ "," ++ showListAsCSV xs

countMistakes (P.State _ pts) = foldl (\c (y,y') -> if y/=y' then c+1 else c) 0 pts

testMistakes classifier predictor examples test = length . filter (\(x,y) -> x/=y)
                                                   $ zip ls predicted where
  predicted = map (\xt -> fromEnum $ predictor learned xs xt) ys
  ls = fst $ unzip pts
  learned@(P.State _ pts) = classifier examples
  xs = fst $ unzip examples
  ys = fst $ unzip test

main :: IO ()
main = do
  -- Parse options, getting a list of option actions
  -- args <- getArgs
  opts <- parseOptions =<< getArgs
  let Options { optVerbose = verbose
              , optInputTrainingVectors = inputTrainingVectors
              , optInputTrainingLabels = inputTrainingLabels
              , optInputVectors = inputVectors
              , optClassifier = classifier
              , optPredictor = predictor
              , optSigma = sigma
              , optOutputWeight = outputWeight
              , optOutputPredictions = outputPredictions
              , optCrossValidate = crossValidate
              , optCrossValidateCount = crossValidateCount
              } = opts 

  {- Read Examples -}
  -- Normalized vectors
  vs <- fmap ( map (P.normalize . V.fromList . map (\x -> read [x] :: Double) . filter (/= ' '))
             . lines) inputTrainingVectors
  -- Labels
  ls <- fmap (map (\x -> fromEnum $ (read x :: Int) > 0) . lines) inputTrainingLabels
  -- examples is a list of vector/label tuples
  let examples = zip vs ls
  let exampleCount = length examples
  let dim = V.length . fst $ head examples
  when verbose (hPutStrLn stderr $ "# Reading " ++ show exampleCount ++ " examples of "
                                ++ show dim ++ "-dimensional vectors")

  {- Cross Validation -}
  when crossValidate (hPutStrLn stderr "\n# Begin cross validations...")
  -- shufflesOfExamples is a list of shuffled examples
  let shufflesOfExamples = take crossValidateCount $ iterate shuffle examples
  -- Take 9 of these shuffles, we will split each from 90%/10% to 10%/90%
  let splits = [fromIntegral exampleCount * (10-a) / 10 | a <- [1..9]]
  let validations = map (\s -> map (\x -> splitAt (round s) x) shufflesOfExamples) splits
  let avgTestMistakes = map ( sum
                            . map (\(x,y) ->
                              testMistakes (classifier sigma) (predictor sigma) x y)) validations
  when crossValidate (do hPutStrLn stderr $ "# Averaging "
                           ++ show crossValidateCount ++ " randomized inputs)"
                         hPutStrLn stderr $ showListAsCSV avgTestMistakes)

  {- Learning -}
  when (verbose) (hPutStrLn stderr "\n# Begin learning...")
  let learned@(P.State ws@(w:_) pts) = classifier sigma examples
  let dim' = V.length w
  let weightCSV = showListAsCSV $ V.toList w
  let mistakes = "# "++(show $ foldl (\c (y,y') -> if y/=y' then c+1 else c) 0 pts)++" mistakes"
  when verbose (hPutStrLn stderr mistakes)
  when verbose (do hPutStrLn stderr $ "# Learned " ++ show dim' ++ "-dimensional weight vector:"
                   hPutStrLn stderr weightCSV)
  outputWeight $ mistakes ++ "\n" ++ weightCSV

  {- Predictions -}
  when (verbose) (hPutStrLn stderr "\n# Begin predictions...")
  ts <- fmap ( map (P.normalize . V.fromList . map (\x -> read [x] :: Double) . filter (/= ' '))
             . lines) inputTrainingVectors
  let testCount = length ts
  let dim'' = V.length $ head ts
  when verbose (hPutStrLn stderr $ "# Reading " ++ show testCount ++ " examples of "
                                ++ show dim'' ++ "-dimensional vectors")
  let ps = map (fromEnum . predictor sigma learned vs) ts
  let pCSV = showListAsCSV ps
  when verbose (do hPutStrLn stderr $ "# Made the following predictions"
                   hPutStrLn stderr $ pCSV)
  outputPredictions pCSV
