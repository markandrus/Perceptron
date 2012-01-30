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

{- Main -}

showListAsCSV :: Show a => [a] -> String
showListAsCSV [x] = show x
showListAsCSV (x:xs) = show x ++ "," ++ showListAsCSV xs

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
              , optSigma = sigma
              , optOutputWeight = outputWeight
              , optOutputPredictions = outputPredictions
              } = opts 
  vs <- fmap ( map (P.normalize . V.fromList . map (\x -> read [x] :: Double) . filter (/= ' '))
             . lines) inputTrainingVectors
  ls <- fmap (map (\x -> fromEnum $ (read x :: Int) > 0) . lines) inputTrainingLabels
  let examples = zip vs ls
  let n = V.length . fst $ head examples
  when verbose (hPutStrLn stderr $ "# Reading " ++ show (length examples) ++ " examples of "
                                ++ show n ++ "-dimensional vectors")
  let (P.State ws@(w:_) pts) = classifier sigma examples
  let n = V.length w
  let wAsCSV = showListAsCSV $ V.toList w
  let mistakes = "# "++(show $ foldl (\c (y,y') -> if y/=y' then c+1 else c) 0 pts)++" mistakes"
  when verbose (hPutStrLn stderr mistakes)
  when verbose (do hPutStrLn stderr $ "# Learned " ++ show n ++ "-dimensional weight vector:"
                   hPutStrLn stderr wAsCSV)
  outputWeight $ mistakes ++ "\n" ++ wAsCSV
