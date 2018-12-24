-- Parse MusicXML, extract score information


module MusicXML where

import System.IO
import Data.Maybe
import qualified Data.Map as M
import Data.ByteString (hGetContents)
import Text.XML.Light
import Text.XML.Light.Helpers

data ScoreData = ScoreData deriving (Show)

data Score = Score {
  scoredata :: Maybe ScoreData
 ,partMap :: M.Map String String
 ,parts :: [Part]
} deriving (Show)

data Part = Part {
  partId :: String
 ,partName :: String
 ,measures :: [Measure]
} deriving (Show)

data Measure = Measure {
  number :: Int
 ,divisions :: Int
 ,timesig :: (Int, Int)
 ,tempo :: Int
 ,lyrnotes :: [LyrNote]
} deriving (Show)

data LyrNote = LyrNote {
  rest :: Bool
 ,step :: String
 ,octave :: Int
 ,alter :: Int
 ,duration :: Int
 ,lsyll :: String
 ,ltext :: String
 ,accent :: Bool
 ,msrnum :: Int
} deriving (Show)

parseMusicXML :: String -> IO Score

parseMusicXML path = do
  h <- openFile path ReadMode
  mxml <- Data.ByteString.hGetContents h
  let mbe = parseXMLDoc mxml
  case mbe of
    Nothing -> error $ "cannot parse " ++ path
    Just e -> return $ score e
 
-- Extract score

score :: Element -> Score

score e = s where
  ps = fromMaybe [] $ mapChildren "part" e part
  mbpl = findChild (unqual "part-list") e
  pmp = partmap mbpl
  ps' = map (\p -> p {partName = fromMaybe (partId p) $ M.lookup (partId p) pmp}) ps
  s = Score Nothing pmp ps'

-- Map of parts and part names

partmap :: Maybe Element -> M.Map String String

partmap Nothing = M.empty

partmap (Just e) = m where
  sps = fromMaybe [] $ mapChildren "score-part" e op
  op p = case findAttr (unqual "id") p of
    Nothing -> Nothing
    Just pid -> Just (pid, pname) where
      pname = fromMaybe pid $ getChildData "part-name" p
  m = M.fromList sps

-- Extract part

part :: Element -> Maybe Part

part e = p where
  mbpid = findAttr (unqual "id") e
  msrs = mapChildren "measure" e measure
  p = Just $ Part (fromMaybe "" mbpid) "" (fromMaybe [] msrs)
  
measure :: Element -> Maybe Measure

measure e = m where
  [mbattr, mbdir, mbsnd] = map (flip findChild e . unqual) ["attributes", "direction", "sound"]
  tmpo = case (mbsnd, mbdir) of
    (Just snd, _) -> getAttrIntData (-1) (unqual "tempo") snd
    (Nothing, Nothing) -> -1
    (Nothing, Just dir) -> case findChild (unqual "sound") dir of
      Nothing -> -1
      Just snd ->  getAttrIntData (-1) (unqual "tempo") snd
  (divs, ts) = case mbattr of
    Nothing -> (-1, (0, 0))
    Just attr ->
      let divs = getChildIntData (-1) "divisions" attr
          mbtm = findChild (unqual "time") attr
          ts = case mbtm of
            Nothing -> (0, 0)
            Just tm ->
              let bts = getChildIntData 0 "beats" tm
                  btt = getChildIntData 0 "beat-type" tm
              in  (bts, btt)
      in (divs, ts)
  msrn = getAttrIntData (-1) (unqual "number") e
  m = Just $ Measure {
             number = msrn
            ,divisions = divs
            ,timesig = ts
            ,tempo = tmpo
            ,lyrnotes = fromMaybe [] $ mapChildren "note" e (note msrn)
           }
  
note :: Int -> Element -> Maybe LyrNote

note m e = n where
  [mbrest, mbpe, mblyr] = map (flip findChild e . unqual) ["rest", "pitch", "lyric"]
  acc = findChild (unqual "notations") e >>= 
        findChild (unqual "articulations") >>=
        findChild (unqual "accent")
  (st, oc, al) = case mbpe of
    Nothing -> ("A", -1, 0)
    Just pe ->
      let st = fromMaybe "A" $ getChildData "step" pe
          oc = getChildIntData (-1) "octave" pe
          al = getChildIntData 0 "alter" pe
      in  (st, oc, al)
  (lsyll, ltext) = case mblyr of
     Nothing -> ("", "")
     Just lyr -> 
       let [lsyll, ltext] = map (fromMaybe "" . flip getChildData lyr) ["syllabic", "text"]
       in  (lsyll, ltext)
  n = Just $ LyrNote {
             rest = isJust mbrest
            ,step = st
            ,octave = oc
            ,alter = al
            ,duration = getChildIntData (-1) "duration" e
            ,lsyll = lsyll
            ,ltext = ltext
            ,msrnum = m
            ,accent = isJust acc
           }


getChildIntData :: Int -> String -> Element -> Int

getChildIntData v c e = z where
  mbcd = getChildData c e
  z = case mbcd of
    Nothing -> v
    Just nc -> case (reads nc) :: [(Double, String)] of
      (nn, _):_ -> round nn
      [] -> v

getAttrIntData :: Int -> QName -> Element -> Int

getAttrIntData v c e = z where
  mbcd = findAttr c e
  z = case mbcd of
    Nothing -> v
    Just nc -> case (reads nc) :: [(Double, String)] of
      (nn, _):_ -> round nn
      [] -> v


