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

main :: IO ()
main = do
  -- Parse options, getting a list of option actions
  -- args <- getArgs
  opts <- parseOptions =<< getArgs
  let Options { optVerbose = verbose
              , optInputVectors = inputVectors
              , optInputLabels = inputLabels
              , optPerceptron = perceptron
              , optSigma = sigma
              , optOutput = output
              } = opts 

  when verbose (hPutStrLn stderr "# Parsing vectors and labels")
  vs <- fmap ( map (V.fromList . map (\x -> read [x] :: Double) . filter (/= ' '))
             . lines) inputVectors
  ls <- fmap (map (\x -> fromEnum $ (read x :: Int) > 0) . lines) inputLabels
  let examples = zip vs ls
  let n = V.length . fst $ head examples
  when verbose (hPutStrLn stderr $ "# Counted " ++ (show $ length examples) ++ " examples of "
                                ++ (show n) ++ "-dimensional vectors")
  let (P.State ws@(w:_) pts) = perceptron sigma examples
  let n = V.length w
  when verbose (do hPutStrLn stderr $ "# Learned " ++ (show n) ++ "-dimensional weight vector:"
                   hPutStrLn stderr . show $ V.toList w)
  let mistakes = foldl (\c (y,y') -> if y /= y' then c+1 else c) 0 pts
  when verbose (hPutStrLn stderr $ "# Made " ++ (show mistakes) ++ " mistakes")
