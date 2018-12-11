-- Generate audio from the parsed score (notes + lyrics)


module GenAudio where

import Data.Maybe
import Control.Monad.State
import Data.WAVE
import Data.Ratio

import Calibrate
import MusicXML
import ExtProg
import NoteGen
import Notes
import Util

-- Process the parsed score measure by measure, leave the generated audio in the state structure.

procScore :: GS ()

procScore = do
  msrs <- gets msrsLeft
  case msrs of
    [] -> return ()
    msr:msrr -> do
      updtempo msr
      makenotes $ lyrnotes msr
      modify (\s -> s {msrsLeft = msrr})
      procScore
  return ()

-- Check if a measure contains tempo changes and update the state

updtempo :: Measure -> GS ()

updtempo cmsr = do
  case divisions cmsr of
    (-1) -> return ()
    dv -> modify (\s -> s {saveDiv = dv})
  case tempo cmsr of
    (-1) -> return ()
    tm -> modify (\s -> s {saveTempo = tm})
  newdiv <- gets saveDiv
  newtmp <- gets saveTempo
  acc <- gets accel
  dec <- gets decel
  let acdec = (fromIntegral dec) % (fromIntegral acc)
  let divlen = 60 % (fromIntegral $ newdiv * newtmp) * acdec
  modify (\s -> s {divLength = divlen})
  return () 

-- Generate a silence wave of goven length in seconds

makepause :: Rational -> GS WAVE

makepause durgen = do
  so <- gets soundOut
  let nfrmrat = (fromIntegral $ waveFrameRate $ waveHeader so) * durgen
      w = so {waveSamples = replicate (round nfrmrat) [0]}
  return w

-- Generate audio for all notes in the given measure.
-- Calculate target time, compare with actual time, calculate the difference
-- Find two synthesized speeds for slightly shorter and slightly longer waves
-- (as there is little chance to generate a wave of exactly needed length)
-- choose the wave closest by duration. Update actual time with actual
-- length of the audio.

makenotes :: [LyrNote] -> GS ()

makenotes [] = return ()

makenotes (lnt:lnts) = do
  divl <- gets divLength
  divs <- gets saveDiv
  trsp <- gets transpose
  detn <- gets detune
  let dursec = (fromIntegral (duration lnt) % 1) * divl
  let tnt = stepAlter (step lnt) (alter lnt)
  cal <- gets caliber
  so <- gets soundOut
  [targ, act] <- mapM gets [targTime, actTime]
  let newtarg = targ + dursec
  let durgen = max 0.0 (newtarg - act)
  (long, short) <- case rest lnt of
    False -> do
      (synth, fund) <- lift $ findNoteSynthPitchTrans (Just (trsp + detn)) cal (octave lnt, tnt)
      modify (\s -> s {caliber = Right fund})
      a <- gets voiceAmpl
      v <- gets voiceName
      (s, l) <- lift $ findSpeed a v synth durgen (ltext lnt) (0, 500)
      [sw, lw] <- lift $ mapM (\t -> runEspeak a v synth t (ltext lnt) >>= getSample) [s, l]
      return (sw, lw)
    True -> do
      w <- makepause durgen   
      return (w, w)
  let [sl, ll] = map soundLength [short, long]
  let [difs, difl] = map (abs . (durgen -)) [sl, ll]
  let (chw, chl) = case compare difs difl of
                     EQ -> (short, sl)
                     GT -> (long, ll)
                     LT -> (short, sl)
  let newact = act + chl
  let newso = concatWaves so chw
  let drift = abs (newtarg - newact)
  let eighth = divl * fromIntegral divs / 2
  (newsodr, newactdr) <- if drift > eighth
    then do
      lift $ putStrLn $ "Warning: time drift exceeds 1/8 at measure " ++ show (msrnum lnt)
      if newtarg > newact
        then do
          p <- makepause (newtarg - newact)
          return (concatWaves newso p, newact + soundLength p)
        else return (newso, newact)
    else return (newso, newact)
  mdr <- gets maxDrift
  if drift > mdr
    then modify (\s -> s {maxDrift = drift})
    else return ()
  modify (\s -> s {actTime = newactdr, targTime = newtarg, soundOut = newsodr})
  makenotes lnts


data GenState = GenState {
  soundOut ::  WAVE                      -- generated sound as concatenation of many waves
 ,lastUtter :: String                    -- text of the last utterance
 ,lastPitch :: Int                       -- pitch of the last utterance
 ,caliber :: Calibration                 -- voice calibration data
 ,voiceName :: String                    -- voice to use with Espeak
 ,voiceAmpl :: Int                       -- voice amplitude
 ,divLength :: Rational                  -- length of one division in seconds
 ,accel :: Int                           -- acceleration factor
 ,decel :: Int                           -- deceleration factor
 ,transpose :: Int                       -- transpose by this N of semitones
 ,detune :: Int                          -- detune (voice only) by this N of semitones
 ,targTime :: Rational                   -- target time for output sound length
 ,actTime :: Rational                    -- actual time for output sound length (reconcillation)
 ,msrsLeft :: [Measure]                  -- measures left
 ,saveDiv :: Int                         -- saved divisions per measure
 ,saveTempo :: Int                       -- saved tempo
 ,maxDrift :: Rational                   -- maximum time drift detected
}

type GS = StateT GenState IO

-- Initialize Generator State from score. Only the first defined part can be used for now.

initGenState :: Score -> IO GenState

initGenState sc = do
  nullsnd <- genSound 0.00000001 0 >>= getSample
  let part = head $ parts sc
      msrs = measures part
  return GenState {
    voiceName = "default"
   ,voiceAmpl = 120
   ,soundOut = nullsnd {waveSamples = []}
   ,lastUtter = ""
   ,lastPitch = (-1)
   ,caliber = Left ("default", "ee")
   ,divLength = 0
   ,accel = 1
   ,decel = 1
   ,transpose = 0
   ,detune = 0
   ,targTime = 0
   ,actTime = 0
   ,msrsLeft = msrs
   ,saveDiv = 0
   ,saveTempo = 0
   ,maxDrift = 0
  }


