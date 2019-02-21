module ExtProg where

import System.Process
import System.Environment
import Data.WAVE
import Data.Ratio
import Numeric



-- Run espeak with given executable, amplitude, voice, pitch, speed, and utterance

runEspeak :: String -> Int -> String -> Int -> Int -> String -> IO CreateProcess

runEspeak exec ampl voice pitch speed utter = return $ proc exec [
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
    "-q",
    "-V0",
    "-r", "22050",
    "-b", "16", "-L",
    "-t", "wav", "/dev/stdout",
    "synth", show (fromRat dur),
    "sine", show freq
  ]

-- Run sox as pipe to resample audio to 44100 Hz
-- sox -q -t wav /dev/stdin -r 44100 /dev/stdout

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


