module Options
  ( Options(..)
  , startOptions
  , options
  , parseOptions
  ) where

import System
import System.IO
import System.Console.GetOpt

import qualified Data.Vector as V
import qualified Perceptron as P

data Options = Options { optVerbose :: Bool
                       , optInputTrainingVectors :: IO String
                       , optInputTrainingLabels :: IO String
                       , optInputVectors :: IO String
                       , optClassifier :: Double -> [(V.Vector Double, Int)] -> P.State
                       , optSigma :: Double
                       , optOutputWeight :: String -> IO ()
                       , optOutputPredictions :: String -> IO ()
                       }

startOptions :: Options
startOptions = Options { optVerbose = False
                       , optInputTrainingVectors = return ""
                       , optInputTrainingLabels = return ""
                       , optInputVectors = return ""
                       , optClassifier = const P.lp
                       , optSigma = 1.0
                       , optOutputWeight = \x -> return ()
                       , optOutputPredictions = \x -> return ()
                       }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "t" ["in-train-vecs"]
    (ReqArg
      (\arg opt -> return opt { optInputTrainingVectors = readFile arg })
      "FILE")
    "Input training vectors"

  , Option "l" ["in-train-labels"]
    (ReqArg
      (\arg opt -> return opt { optInputTrainingLabels = readFile arg })
      "FILE")
    "Input training labels"

  , Option "i" ["in-vecs"]
    (ReqArg
      (\arg opt -> return opt { optInputVectors = readFile arg })
      "FILE")
    "Input vectors (to classify)"

  , Option "c" ["class"]
    (ReqArg
      (\arg opt -> return opt { optClassifier = case arg of
                                  "lp"     -> const P.lp
                                  "lp2"    -> const P.lp2
                                  "gp"     -> P.gp })
      "[lp | lp2 | gp]")
    "Classifier"

  , Option "s" ["sigma"]
    (ReqArg
      (\arg opt -> return opt { optSigma = read arg })
      "DOUBLE")
    "Sigma value for Guassian kernel (gp)"

  , Option "w" ["out-weight"]
    (ReqArg
      (\arg opt -> return opt { optOutputWeight = writeFile arg })
      "FILE")
    "Output weight path"

  , Option "o" ["out-preds"]
    (ReqArg
      (\arg opt -> return opt { optOutputPredictions = writeFile arg })
      "FILE")
    "Output predictions path"

  , Option "v" ["verbose"]
    (NoArg
      (\opt -> return opt { optVerbose = True }))
    "Enable verbose messages"

  , Option "h" ["help"]
    (NoArg
      (\_ -> do
             prg <- getProgName
             hPutStrLn stderr (usageInfo prg options)
             exitWith ExitSuccess))
    "Show help"
  ]

parseOptions :: [String] -> IO Options
parseOptions args = do
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  foldl (>>=) (return startOptions) actions
