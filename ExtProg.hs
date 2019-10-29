module ExtProg where

import System.Process
import System.Environment
import Data.WAVE
import Data.List
import Data.Ratio
import Numeric



-- Run espeak with given executable, amplitude, voice, pitch, speed, and utterance

runEspeak :: String -> Int -> String -> Int -> Int -> String -> IO CreateProcess

runEspeak exec ampl voice@(v:vs) pitch speed utter = return $
  let ld | v `elem` "./" = ["--load"]
         | otherwise = []
  in  proc exec $ ld ++ [
    "-g", "0",
    "-k", "0",
    "-z",
    "-w", "/dev/stdout",
    "-a", show ampl,
    "-p", show pitch,
    "-s", show speed,
    "-v", voice,
    utter
  ]


rs44100 :: IO CreateProcess

rs44100 = return $ proc "sox" [
    "-q",
    "-t", "wav",
    "/dev/stdin",
    "-r", "44100",
    "-b", "16", "-L",
    "-t", "wav",
    "/dev/stdout"
  ]

-- Get a sound sample from an espeak process (expecting it on its stdout)

getSample :: CreateProcess -> IO WAVE

getSample proc = do
  let proc' = proc { std_out = CreatePipe }
  (_, mbhout, _, p) <- createProcess proc'
  case mbhout of
    Nothing -> error "Cannot get stdout handle"
    Just hout -> do
      w <- hGetWAVE hout
      waitForProcess p
      return w


