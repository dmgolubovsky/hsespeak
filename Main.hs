--	espeak -a 140 -vmono-female -z -g0 -k0 -s$1 -p$2 "$3" -w /dev/stdout | 
--      sox -q -t wav /dev/stdin -r 44100 "$4"

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import WithCli
import System.IO
import Data.WAVE
import qualified Data.Map as M
import Data.List (find, intercalate)
import Data.Maybe
import System.Exit
import System.Environment
import System.Process
import Control.Monad.State
import System.Directory
import System.FilePath
import ZMidi.Core

import Notes
import ExtProg
import Util
import Calibrate
import NoteGen
import MusicXML
import GenAudio



main = withCliModified mods main'

main' :: MXML -> Options -> IO ()

main' (MXML xmlpath') opts = do
  let newvce = fromMaybe "default" (voice' opts)
      newcal = fromMaybe "ee" (calibtxt' opts)
      en = fromMaybe "espeak" (exec' opts)
  if calib' opts
    then do
      vf <- voiceFundamental en newcal 50 newvce
      case vf of
        Just cf -> do
          putStrLn $ show cf ++ " Hz"
          exitSuccess
        Nothing -> exitFailure
    else return ()
  sc <- parseMusicXML xmlpath'
  gs <- initGenState sc
  part <- case parts sc of
    [] -> error "Score has no parts"
    [p] -> return p
    pps | part' opts == Nothing -> do
      let shpts = intercalate ", " (map partName $ parts sc)
      error $ "Multiple parts detected: " ++ shpts ++ "; specify part with -p"
    pps -> do
      let pp = fromMaybe "______" $ part' opts
      let mbprt = find (\p -> partName p == pp) (parts sc)
      case mbprt of
        Nothing -> error $ "No part named " ++ pp ++ " in the score"
        Just prt -> return prt
  let gs' = gs {
    voiceName = newvce
   ,accel = fromMaybe 1 (accel' opts)
   ,decel = fromMaybe 1 (decel' opts)
   ,voiceAmpl = fromMaybe (voiceAmpl gs) (ampl' opts)
   ,accAmpl = fromMaybe (accAmpl gs) (acc' opts)
   ,transpose = fromMaybe 0 (transp' opts)
   ,detune = fromMaybe 0 (detune' opts)
   ,caliber = case cfreq' opts of
      Nothing -> Left (en, newvce, newcal)
      Just cf -> Right cf
   ,msrsLeft = measures part
   ,execName = en
  }
  let stem = takeBaseName xmlpath' ++ case part' opts of
                                        Nothing -> ""
                                        Just ppp -> "_" ++ partName part
  curdir <- getCurrentDirectory
  let outstem = case output' opts of
                  Nothing -> curdir </> stem
                  Just outp -> dropExtension outp
  fings <- execStateT procScore gs'
  let midihdr = MidiHeader {
    hdr_format = MF0
   ,num_tracks = 1
   ,time_division = TPB $ fromIntegral $ midiTempo fings
  }
  let midifile = MidiFile midihdr [MidiTrack $ midiMsg $ fings]
  let outmidi = outstem `addExtension` "mid"
  writeMidi outmidi midifile
  let outwav = outstem `addExtension` "wav"
  putStrLn $ "maximal drift was " ++ show ((fromRational $ maxDrift fings) :: Float) ++ " sec"
  houtw <- openFile outwav WriteMode
  rsox' <- rs44100 
  let rsox = rsox' {
    std_in = CreatePipe
   ,std_out = UseHandle houtw
  }
  (mbhin, _, _, p) <- createProcess rsox
  case mbhin of
    Nothing -> error "cannot get sox input pipe handle"
    Just hin -> do
      hPutWAVE hin (soundOut fings)
      hClose hin
  waitForProcess p
  hClose houtw
  return ()

data MXML = MXML FilePath

instance Argument MXML where
  argumentType Proxy = "path-to-the-Music-XML-file"
  parseArgument f = Just (MXML f)

instance HasArguments MXML where
  argumentsParser = atomicArgumentsParser


data Options = Options {
  accel' :: Maybe Int
 ,decel' :: Maybe Int
 ,transp' :: Maybe Int
 ,detune' :: Maybe Int
 ,ampl' :: Maybe Int
 ,voice' :: Maybe String
 ,calib' :: Bool
 ,calibtxt' :: Maybe String
 ,output' :: Maybe String
 ,part' :: Maybe String
 ,acc' :: Maybe Int
 ,cfreq' :: Maybe Double
 ,exec' :: Maybe String
} deriving (Show, Generic, HasArguments)

mods :: [Modifier]

mods = [
  AddShortOption "voice'" 'v'
 ,AddOptionHelp  "voice'" "Espeak voice name to use"
 ,AddShortOption "calibtxt'" 'c'
 ,AddOptionHelp  "calibtxt'" 
                 "Text to use for voice calibration, default is 'ee' for English voices"
 ,AddShortOption "calib'" 'C'
 ,AddOptionHelp  "calib'" "Calibrate voice only, print central frequency, and exit"
 ,AddShortOption "accel'" 'a'
 ,AddOptionHelp  "accel'" "Accelerate the vocals given number of times, can be used with --decel"
 ,AddShortOption "decel'" 'd'
 ,AddOptionHelp  "decel'" "Decelerate the vocals given number of times, can be used with --accel"
 ,AddShortOption "ampl'" 'A'
 ,AddOptionHelp  "ampl'" "Amplitude of vocals (Espeak parameter), default is 120"
 ,AddShortOption "transp'" 't'
 ,AddOptionHelp  "transp'" "Transpose both vocals and MIDI by given number of semitones"
 ,AddShortOption "detune'" 'D'
 ,AddOptionHelp  "detune'" "Detune vocals only by given number of semitones"
 ,AddShortOption "output'" 'o'
 ,AddShortOption "part'" 'p'
 ,AddOptionHelp  "part'" "Select part to process by part name"
 ,AddShortOption "cfreq'" 'F'
 ,AddOptionHelp  "cfreq'" "Assume central pitch of the voice this value when calibration fails"
 ,AddOptionHelp  "acc'" "Increase amplitude for accented notes by this value, default is 20"
 ,AddShortOption "exec'" 'x'
 ,AddOptionHelp  "exec'" "Synthesizer executable name, default is espeak"
       ]
