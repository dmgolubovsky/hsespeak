module Util where

import Data.List
import Data.WAVE

-- Find local maxima of the list

locMax :: Ord b => [(a, b)] -> [(a, b)]

locMax xy@((x1, y1) : (x2, y2) : (x3, y3) : xys) | y2 > y1 && y2 > y3 = 
  (x2, y2) : locMax (tail xy)

locMax [] = []

locMax xy = locMax (tail xy)

sortAscFst = sortBy $ \(x1, y1) (x2, y2) -> compare x1 x2 


-- Round to given precision

roundTo :: Double -> Double -> Double

roundTo prec n = prec * fromIntegral (round(n / prec))

-- Normalize list: find the maximum and divide every element by it

normalize :: [Double] -> [Double]

normalize l = map ( / m) l where m = maximum l

-- Smoothen the spectrum via moving average

smoothen :: Int -> [(Double, Double)] -> [(Double, Double)]

smoothen k xy = zip (map fst xy') (normalize $ mavg k $ map snd xy')
  where xy' = sortAscFst xy

mavg :: Fractional b => Int -> [b] -> [b]
mavg k lst = map (/ fromIntegral k) $ scanl' (+) (sum h) $ zipWith (-) t lst
  where (h, t) = splitAt k lst 

-- Range search (universal)

rangeSearch :: (Monad m, Num r, Integral r) => r -> (r, r) -> (r -> m Ordering) -> m (r, r)

rangeSearch delta (lo, hi) fun | hi - lo <= delta = return (lo, hi)

rangeSearch delta (lo, hi) fun = do
  let mid = (lo + hi) `div` 2
  res <- fun mid
  case res of
    EQ -> return (mid, mid)
    LT -> rangeSearch delta (lo, mid) fun
    GT -> rangeSearch delta (mid, hi) fun

-- Calculate sound sample length (sec) from WAVE header

soundLength :: WAVE -> Rational

soundLength wav = (fromIntegral $ length $ waveSamples wav) / 
                  (fromIntegral $ waveFrameRate $ waveHeader wav)


-- Concatenate 2 waves (no interpolation for now)

concatWaves :: WAVE -> WAVE -> WAVE

concatWaves w1 w2 = w3 where
  h1 = waveHeader w1
  h2 = waveHeader w2
  compat = waveNumChannels h1 == waveNumChannels h2 &&
           waveFrameRate h1 == waveFrameRate h2 &&
           waveBitsPerSample h1 == waveBitsPerSample h2
  h3 = h1 {waveFrames = Nothing}
  w3 = case compat of
    False -> error "Wave headers not compatible"
    True -> WAVE {
              waveHeader = h3
             ,waveSamples = waveSamples w1 ++ waveSamples w2}
 
