module ExtProg where

import System.Process
import System.Environment
import Data.WAVE
import Data.Ratio
import Numeric

-- Run espeak with given amplitude, voice, pitch, speed, and utterance

runEspeak :: Int -> String -> Int -> Int -> String -> IO CreateProcess

runEspeak ampl voice pitch speed utter = return $ proc "espeak" [
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

-- Run sox to generate a sound of given duration and frequency
-- sox -n -r 22050 -t wav /dev/stdout synth 1.0 sine 220

genSound :: Rational -> Double -> IO CreateProcess

genSound dur freq = return $ proc "sox" [
    "-n",
    "-r", "22050",
    "-b", "16", "-L",
    "-t", "wav", "/dev/stdout",
    "synth", show (fromRat dur),
    "sine", show freq
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


