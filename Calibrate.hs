module Calibrate where

import Data.WAVE
import Data.Complex
import Data.List
import Numeric.FFT

import Util
import ExtProg

-- Calibrated or uncalibrated voice: calibrated means we know the fundamental frequency
-- for pitch 50, uncalibrated means we use the given voice name and text to get it.

type Calibration = Either (String, String, String) Double

-- Determine the fundamental frequency for a voice at the given pitch. Get the spectrum,
-- smoothen it and get the first peak.

voiceFundamental :: String -> String -> Int -> String -> IO (Maybe Double)

voiceFundamental exec text pitch voice = do
  p <- voiceSpectrumPitchText exec text pitch voice
  let smpp = filter ((> 0.05) . snd) $ smoothen 3 p
      lmpp = locMax smpp
      fund = case lmpp of
        [] -> Nothing
        _ -> Just $ fst $ head lmpp
  return fund

-- The most general case: set text, calibration pitch, and voice name.

voiceSpectrumPitchText :: String -> String -> Int -> String -> IO [(Double, Double)]

voiceSpectrumPitchText exec text pitch voice = do
  spd <- findSpeed exec 120 voice pitch 0.25 text (1, 300) >>= return . fst
  wav <- runEspeak exec 120 voice pitch spd text >>= getSample
  let fft = filter ((< 2000) . fst) $ waveFFT wav
  return fft


-- Run a FFT on a signal given WAVE, returning pairs of frequency-normalized magnitude
-- sorted by decreasing magnitude

waveFFT :: WAVE -> [(Double, Double)]

waveFFT wav =
  let sampldbl = map (sampleToDouble . head) $ waveSamples wav
      sampcmplx = map (:+ 0) sampldbl
      freqdom = map magnitude $ fft sampcmplx
      rate = waveFrameRate $ waveHeader wav
      freqs = map (\i -> fromIntegral (i * fromIntegral rate) / fromIntegral (length freqdom))
                  [0 .. length freqdom - 1]
      spectrum = zip freqs (normalize freqdom)
      sortDescSnd = sortBy $ \(x1, y1) (x2, y2) -> compare y2 y1 
  in  sortDescSnd spectrum

-- Find a minimal range of speed that approximates the given time in seconds

findSpeed :: String -> Int -> String -> Int -> Rational -> String -> (Int, Int) -> IO (Int, Int)

findSpeed exec ampl voice pitch tsec utter (lo, hi) = 
  rangeSearch 1 (lo, hi) $ \mid -> do
    wav <- runEspeak exec ampl voice pitch mid utter >>= getSample
    return $ compare (soundLength wav) tsec

 
