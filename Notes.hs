module Notes where

-- Notes and pitches

data Note = C | CSharp | DFlat | D | DSharp | EFlat | E | F | FSharp | GFlat | G | GSharp |
            AFlat | A | ASharp | BFlat | B deriving (Show)

-- Note number within octave

noteNumber :: Note -> Int

noteNumber C = 0
noteNumber CSharp = 1
noteNumber DFlat = 1
noteNumber D = 2
noteNumber DSharp = 3
noteNumber EFlat = 3
noteNumber E = 4
noteNumber F = 5
noteNumber FSharp = 6
noteNumber GFlat = 6
noteNumber G = 7
noteNumber GSharp = 8
noteNumber AFlat = 8
noteNumber A = 9
noteNumber ASharp = 10
noteNumber BFlat = 10
noteNumber B = 11

-- Reverse of noteNumber, returns Sharp if ambiguous

numberNote :: Int -> Note

numberNote 0 = C
numberNote 1 = CSharp
numberNote 2 = D
numberNote 3 = DSharp
numberNote 4 = E
numberNote 5 = F
numberNote 6 = FSharp
numberNote 7 = G
numberNote 8 = GSharp
numberNote 9 = A
numberNote 10 = ASharp
numberNote 11 = B
numberNote _ = error "Incorrect note"

-- Note from step-alter notation used in MusicXML

stepAlter :: String -> Int -> Note

stepAlter "A" = numberNote . (9 +)
stepAlter "B" = numberNote . (11 +)
stepAlter "C" = numberNote . (0 +)
stepAlter "D" = numberNote . (2 +)
stepAlter "E" = numberNote . (4 +)
stepAlter "F" = numberNote . (5 +)
stepAlter "G" = numberNote . (7 +)

-- Pitches for the 4th octave http://pages.mtu.edu/~suits/notefreqs.html

pitches4 :: [Double]

pitches4 = [261.63, 277.18, 293.66, 311.13, 329.63, 349.23,
            369.99, 392.00, 415.30, 440.00, 466.16, 493.88]

-- Note absolute number from octave and note (C4 == 60)

noteAbsNumber :: Int -> Note -> Int

noteAbsNumber oct note = (oct + 1) * 12 + (noteNumber note)

-- Note pitch given octave and note

notePitch :: Int -> Note -> Double

notePitch 4 n = oct4 n where
  oct4 n = pitches4 !! noteNumber n

notePitch oct n = (notePitch 4 n) * (2.0 ** (fromIntegral oct - 4))

type OctNote = (Int, Note)

-- Octave and note given absolute note

octNote :: Int -> OctNote

octNote absn = (oct - 1, note) where
  (oct, note') = quotRem absn 12
  note = numberNote note'


