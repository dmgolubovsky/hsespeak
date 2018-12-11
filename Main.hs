--	espeak -a 140 -vmono-female -z -g0 -k0 -s$1 -p$2 "$3" -w /dev/stdout | 

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import WithCli
import System.IO
import Data.WAVE
import Data.Maybe
import System.Environment
import Control.Monad.State
import System.FilePath

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
  sc <- parseMusicXML xmlpath'
  gs <- initGenState sc
  let newvce = fromMaybe "default" (voice' opts)
      newcal = fromMaybe "ee" (calibtxt' opts)
  let gs' = gs {
    voiceName = newvce
   ,accel = fromMaybe 1 (accel' opts)
   ,decel = fromMaybe 1 (decel' opts)
   ,voiceAmpl = fromMaybe (voiceAmpl gs) (ampl' opts)
   ,transpose = fromMaybe 0 (transp' opts)
   ,detune = fromMaybe 0 (detune' opts)
   ,caliber = Left (newvce, newcal)
  }
  fings <- execStateT procScore gs'
  putStrLn $ "maximal drift was " ++ show ((fromRational $ maxDrift fings) :: Float)
  putWAVEFile "soundOut.wav" (soundOut fings)
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
 ,calibtxt' :: Maybe String
} deriving (Show, Generic, HasArguments)

mods :: [Modifier]

mods = [
  AddShortOption "voice'" 'v'
 ,AddShortOption "calibtxt'" 'c'
 ,AddShortOption "accel'" 'a'
 ,AddShortOption "decel'" 'd'
 ,AddShortOption "ampl'" 'A'
 ,AddShortOption "transp'" 't'
 ,AddShortOption "detune'" 'D'
       ]
