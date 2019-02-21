module NoteGen where

import Data.Functor.Identity
import Data.Maybe

import Notes
import ExtProg
import Util
import Calibrate

-- Find a range of notes around the given frequency, then find the closest of the two

findNote :: Double -> (Int, Note)

findNote freq = octNote $ sel $ rng where
  rng@(nlow, nhigh) = findNoteRange freq
  ft f (a, b) = f a b
  flow = ft notePitch $ octNote nlow
  fhigh = ft notePitch $ octNote nhigh
  sel = case compare (freq - flow) (fhigh - freq) of
    EQ -> fst
    LT -> fst
    GT -> snd

-- Find a minimal range of notes that approximate the given frequency

findNoteRange :: Double -> (Int, Int)

findNoteRange freq = runIdentity $ rangeSearch 1 (0::Int, 127::Int) $ \mid -> do
  let (oct, note) = octNote mid
      nfrq = notePitch oct note
  return $ compare freq nfrq

-- Approximate the synthesizer pitch to generate a given note for a given voice.
-- A fundamental frequency for pitch 50 may be supplied or not; if not supplied it will be 
-- determined (the voice calibrated) and returned as part of result (not repeating
-- calibration every time as it is a time consuminng process). Distance (semitones) 
-- will be calculated between the given note and the note closest to the fundamental 
-- frequency at pitch 50, and the synth pitch approximated using linear scale 
-- as 50/12 per semitone.

findNoteSynthPitch = findNoteSynthPitchTrans Nothing

findNoteSynthPitchTrans :: Maybe Int -> Calibration -> OctNote -> IO (Int, Double)

findNoteSynthPitchTrans mbtr cal (toct, tnote) = do
  fund <- case cal of
    Left (exec, voice, text) -> 
      do
        vf <- voiceFundamental exec text 50 voice
        case vf of
          Just ff -> return ff
          Nothing -> error $ "Cannot calibrate voice " ++ voice
    Right ff -> return ff
  let (oct, note) = findNote fund
      acnote = noteAbsNumber oct note
      atnote = noteAbsNumber toct tnote + fromMaybe 0 mbtr
      ndiff = atnote - acnote
      pdiff = (ndiff * 50) `div` 12
  if abs(pdiff) > 50
    then error $ "Note " ++ show tnote ++ show toct ++ 
                 " outside the range of voice (CF " ++ show fund ++ " Hz)"
    else return (50 + pdiff, fund)


