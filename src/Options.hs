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
                       , optInputVectors :: IO String
                       , optInputLabels :: IO String
                       , optPerceptron :: Double -> [(V.Vector Int, Int)] -> P.State
                       , optSigma :: Double
                       , optOutput :: String -> IO ()
                       }

startOptions :: Options
startOptions = Options { optVerbose = False
                       , optInputVectors = return ""
                       , optInputLabels = return ""
                       , optPerceptron = const P.lp
                       , optSigma = 1.0
                       --, optOutput = writeFile "labels.txt"
                       , optOutput = putStrLn
                       }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "i" ["vectors"]
    (ReqArg
      (\arg opt -> return opt { optInputVectors = readFile arg })
      "FILE")
    "Input vectors"

  , Option "l" ["labels"]
    (ReqArg
      (\arg opt -> return opt { optInputLabels = readFile arg })
      "FILE")
    "Input labels"

  , Option "p" ["perc"]
    (ReqArg
      (\arg opt -> return opt { optPerceptron = case arg of
                                  "lp"     -> const P.lp
                                  "lp2"    -> const P.lp2
                                  "gp"     -> P.gp })
      "lp|lp2|gp")
    "Perceptron algorithm"

  , Option "s" ["sigma"]
    (ReqArg
      (\arg opt -> return opt { optSigma = read arg })
      "FILE")
    "Sigma value to use with Guassian perceptron"

  , Option "o" ["out"]
    (ReqArg
      (\arg opt -> return opt { optOutput = writeFile arg })
      "FILE")
    "Output path"

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
