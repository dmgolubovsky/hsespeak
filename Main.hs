--	espeak -a 140 -vmono-female -z -g0 -k0 -s$1 -p$2 "$3" -w /dev/stdout | 
--      sox -q -t wav /dev/stdin -r 44100 "$4"

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import WithCli
import System.IO
import Data.WAVE
import Data.Maybe
import System.Environment
import System.Process
import Control.Monad.State
import System.Directory
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
  let stem = takeBaseName xmlpath'
  curdir <- getCurrentDirectory
  let outstem = case output' opts of
                  Nothing -> curdir </> stem
                  Just outp -> dropExtension outp
  fings <- execStateT procScore gs'
  let outwav = outstem `addExtension` "wav"
  putStrLn $ "maximal drift was " ++ show ((fromRational $ maxDrift fings) :: Float)
  houtw <- openFile outwav WriteMode
  rsox' <- rs44100 
  let rsox = rsox' {
    std_in = CreatePipe
   ,std_out = UseHandle houtw
  }
  (mbhin, _, _, p) <- createProcess rsox
  case mbhin of
    Nothing -> error "cannot get sox input pipe handle"
    Just hin -> hPutWAVE hin (soundOut fings)
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
 ,calibtxt' :: Maybe String
 ,output' :: Maybe String
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
 ,AddShortOption "output'" 'o'
       ]
