module Audio where

import Data.Int
import SDL.Audio

initAudio :: IO ()
initAudio = void $
  openAudioDevice $
  OpenDeviceSpec
    (Mandate 44100)
    (Mandate Signed8BitAudio)
    (Mandate Mono)
    65536
    cb
    ForPlayback
    Nothing

wave :: [Int8]

cb :: AudioFormat a -> IOVector a -> IO ()
cb Signed8BitAudio vec =
  [0..65535] `forM_` \x -> write vec x (wave !! x)
