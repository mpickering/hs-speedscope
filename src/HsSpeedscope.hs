{-# LANGUAGE OverloadedStrings #-}
module HsSpeedscope where


import Data.Aeson
import GHC.RTS.Events

import Data.Word
import Data.Text (Text)
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import System.Environment
import Data.Maybe


entry :: IO ()
entry = do
  [fp] <- getArgs
  el <- either error id <$> readEventLogFromFile fp
  encodeFile (fp ++ ".json") (convertToSpeedscope el)

convertToSpeedscope :: EventLog -> Value
convertToSpeedscope (EventLog h (Data es)) =
  object [ "version" .= ("0.0.1" :: String)
         , "$schema" .= ("https://www.speedscope.app/file-format-schema.json" :: String)
         , "shared" .= object [ "frames" .= frames_json ]
         , "profiles" .= [object [ "type" .= ("sampled" :: String)
                                , "unit" .= ("none" :: String)
                                , "name" .= ("test" :: String)
                                , "startValue" .= (0 :: Int)
                                , "endValue" .= (length samples :: Int)
                                , "samples" .= events_json
                                , "weights" .= sample_weights ]]
         ]
  where
    (frames, samples) = foldr processEvents ([], []) es

    -- Drop 7 events for GC

    frames_json :: [Value]
    frames_json = map mkFrame (reverse (drop 7 frames))

    num_frames = length frames_json

    events_json :: [[Int]]
    events_json = mapMaybe mkSample samples

    sample_weights :: [Int]
    sample_weights = replicate (length events_json) 1

    mkFrame :: Frame -> Value
    mkFrame (Frame n l) = object [ "name" .= l ]

    mkSample :: Sample -> Maybe [Int]
    -- Filter out system frames
    mkSample (Sample ti [k]) | fromIntegral k >= num_frames = Nothing
    mkSample (Sample ti ccs) = Just $ reverse $ map (subtract 1 . fromIntegral) ccs


    processEvents :: Event -> ([Frame], [Sample]) -> ([Frame], [Sample])
    processEvents (Event t ei c) (fs, cs) =
      case ei of
        HeapProfCostCentre n l _m _s _ -> (Frame n l : fs, cs)
        ProfSampleCostCentre t _ st -> (fs, Sample t (V.toList st) : cs)
        _ -> (fs, cs)


data Frame = Frame Word32 Text

data Sample = Sample Word64 [Word32]





