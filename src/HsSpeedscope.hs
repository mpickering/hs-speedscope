{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module HsSpeedscope where


import Data.Aeson
import GHC.RTS.Events

import Data.Word
import Data.Text (Text)
import Data.Foldable
import qualified Data.Vector.Unboxed as V
import System.Environment
import Data.Maybe
import Data.List.Extra

import Data.Version
import qualified Paths_hs_speedscope as Paths

entry :: IO ()
entry = do
  [fp] <- getArgs
  el <- either error id <$> readEventLogFromFile fp
  encodeFile (fp ++ ".json") (convertToSpeedscope el)

convertToSpeedscope :: EventLog -> Value
convertToSpeedscope (EventLog h (Data es)) =
  object [ "version" .= ("0.0.1" :: String)
         , "$schema" .= ("https://www.speedscope.app/file-format-schema.json" :: String)
         , "shared" .= object [ "frames" .= ccs_json ]
         , "profiles" .= map (mkProfile name interval) caps
         , "name" .= name
         , "activeProfileIndex" .= (0 :: Int)
         , "exporter" .= version_string
         ]
  where
    (fromMaybe "" -> name, fromMaybe 1 -> interval, frames, samples) =
      foldr processEvents (Nothing, Nothing, [], []) es

    version_string :: String
    version_string = showVersion Paths.version

    -- Drop 7 events for built in cost centres like GC

    ccs_json :: [Value]
    ccs_json = map mkFrame (reverse (drop 7 frames))

    num_frames = length ccs_json


    caps :: [(Capset, [[Int]])]
    caps = groupSort $ mapMaybe mkSample samples

    mkFrame :: CostCentre -> Value
    mkFrame (CostCentre n l m s) = object [ "name" .= l, "file" .= s ]

    mkSample :: Sample -> Maybe (Capset, [Int])
    -- Filter out system frames
    mkSample (Sample ti [k]) | fromIntegral k >= num_frames = Nothing
    mkSample (Sample ti ccs) = Just $ (ti, reverse $ map (subtract 1 . fromIntegral) ccs)


    processEvents :: Event -> (Maybe String, Maybe Word64, [CostCentre], [Sample]) -> (Maybe String, Maybe Word64, [CostCentre], [Sample])
    processEvents (Event t ei c) (mn, mi, fs, cs) =
      case ei of
        ProgramArgs _ (prog_name: _args) -> (Just prog_name, mi, fs, cs)
        ProfBegin interval -> (mn, Just interval, fs, cs)
        HeapProfCostCentre n l m s _ -> (mn, mi, CostCentre n l m s : fs, cs)
        ProfSampleCostCentre t _ _ st -> (mn, mi, fs, Sample t (V.toList st) : cs)
        _ -> (mn, mi, fs, cs)

mkProfile :: String -> Word64 -> (Capset, [[Int]]) -> Value
mkProfile prog_name interval (n, samples) =
  object [ "type" .= ("sampled" :: String)
         , "unit" .= ("nanoseconds" :: String)
         , "name" .= prog_name
         , "startValue" .= (0 :: Int)
         , "endValue" .= (length samples :: Int)
         , "samples" .= samples
         , "weights" .= sample_weights ]
  where
    sample_weights :: [Word64]
    sample_weights = replicate (length samples) interval

data CostCentre = CostCentre Word32 Text Text Text

data Sample = Sample Capset [Word32]





