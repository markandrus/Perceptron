module Main (main) where
 
import System
import System.IO
import System.IO.Error

import Control.Exception (bracket, bracket_)
import Control.Monad
import Control.Arrow

import qualified Data.Vector as V

import Options
import Perceptron

-- From the Control.Functor.Zip package
class Functor f => Zip f where
  fzip :: f a -> f b -> f (a, b)
  fzip = fzipWith (,)
  fzipWith :: (a -> b -> c) -> f a -> f b -> f c
  fzipWith f as bs = fmap (uncurry f) (fzip as bs)

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
  vs <- fmap (map (V.fromList . map (\x -> read [x] :: Integer)) . lines) inputVectors
  ls <- fmap (map (map (\x -> read [x] :: Integer)) . lines) inputLabels
  let examples = zip vs ls
  when verbose (hPutStrLn stderr $ show examples)
