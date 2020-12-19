{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module HsSpeedscope where

import Data.String ( fromString )
import Control.Monad
import Data.Aeson
import Data.Char
import Data.List.Extra
import Data.Maybe
import qualified Data.Text
import Data.Text (Text)
import qualified Data.Vector.Unboxed as V
import Data.Version
import Data.Word
import GHC.RTS.Events hiding (header, str)
import qualified Options.Applicative as O
import Options.Applicative hiding (optional)
import qualified Paths_hs_speedscope as Paths
import Speedscope.Schema
import Text.ParserCombinators.ReadP


data SSOptions = SSOptions { file :: FilePath
                       , isolateStart :: Maybe Text
                       , isolateEnd :: Maybe Text
                       } deriving Show


optsParser :: Parser SSOptions
optsParser = SSOptions
  <$> argument str (metavar "FILE.eventlog")
  <*> O.optional (strOption
    ( short 's'
    <> long "start"
    <> metavar "STRING"
    <> help "No samples before the first eventlog message with this prefix will be included in the output" ))
  <*> O.optional (strOption
    ( short 'e' <> long "end" <> metavar "STRING" <> help "No samples after the first eventlog message with this prefix will be included in the output" ))



entry :: IO ()
entry = do
  os <- execParser opts
  run os
  where
    opts = info (optsParser <**> helper)
      ( fullDesc
     <> progDesc "Generate a speedscope.app json file from an eventlog"
     <> header "hs-speedscope" )

run :: SSOptions -> IO ()
run SSOptions{ file, isolateStart, isolateEnd } = do
  el <- either error id <$> readEventLogFromFile file
  encodeFile (file ++ ".json") (convertToSpeedscope (isolateStart, isolateEnd ) el)

data ReadState =
        ReadAll -- Ignore all future
      | IgnoreUntil Text ReadState
      | ReadUntil Text ReadState
      | IgnoreAll deriving Show

shouldRead :: ReadState -> Bool
shouldRead ReadAll = True
shouldRead (ReadUntil {}) = True
shouldRead _ = False

transition :: Text -> ReadState -> ReadState
transition s r = case r of
                   (ReadUntil is n) | is `Data.Text.isPrefixOf` s -> n
                   (IgnoreUntil is n) | is `Data.Text.isPrefixOf` s -> n
                   _ -> r

initState :: Maybe Text -> Maybe Text -> ReadState
initState Nothing Nothing = ReadAll
initState (Just s) e = IgnoreUntil s (initState Nothing e)
initState Nothing  (Just e) = ReadUntil e IgnoreAll

convertToSpeedscope :: (Maybe Text, Maybe Text) -> EventLog -> Value
convertToSpeedscope (is, ie) (EventLog _h (Data (sortOn evTime -> es))) =
  case el_version of
    Just (ghc_version, _) | ghc_version < makeVersion [8,9,0]  ->
      error ("Eventlog is from ghc-" ++ showVersion ghc_version ++ " hs-speedscope only works with GHC 8.10 or later")
    _ -> toJSON file
      where
        file = File
          { shared             = Shared{ frames = ccs_json }
          , profiles           = map (mkProfile profile_name interval) caps
          , name               = Just profile_name
          , activeProfileIndex = Just 0
          , exporter           = Just $ fromString version_string
          }
  where
    (EL (fromMaybe "" -> profile_name) el_version (fromMaybe 1 -> interval) frames samples) =
      snd $ foldl' (flip processEvents) (initState is ie, initEL) es

    initEL = EL Nothing Nothing Nothing [] []


    version_string :: String
    version_string = "hs-speedscope@" ++ showVersion Paths.version

    -- Drop 7 events for built in cost centres like GC, IDLE etc
    ccs_raw = reverse (drop 7 (reverse frames))


    ccs_json :: [Frame]
    ccs_json = map mkFrame ccs_raw

    num_frames = length ccs_json


    caps :: [(Capset, [[Int]])]
    caps = groupSort $ mapMaybe mkSample (reverse samples)

    mkFrame :: CostCentre -> Frame
    mkFrame (CostCentre _n name _m file) = Frame{ name, file = Just file, col = Nothing, line = Nothing }

    mkSample :: Sample -> Maybe (Capset, [Int])
    -- Filter out system frames
    mkSample (Sample _ti [k]) | fromIntegral k >= num_frames = Nothing
    mkSample (Sample ti ccs) = Just (ti, map (subtract 1 . fromIntegral) (reverse ccs))


    processEvents :: Event -> (ReadState, EL) -> (ReadState, EL)
    processEvents (Event _t ei _c) (do_sample, el) =
      case ei of
        ProgramArgs _ (pname: _args) ->
          (do_sample, el { prog_name = Just pname })
        RtsIdentifier _ rts_ident ->
          (do_sample, el { rts_version = parseIdent rts_ident })
        ProfBegin ival ->
          (do_sample, el { prof_interval = Just ival })
        HeapProfCostCentre n l m s _ ->
          (do_sample, el { cost_centres = CostCentre n l m s : cost_centres el })
        ProfSampleCostCentre t _ _ st ->
          if shouldRead do_sample then
            (do_sample, el { el_samples = Sample t (V.toList st) : el_samples el })
            else (do_sample, el)
        (UserMarker m) -> (transition m do_sample, el)
        _ -> (do_sample, el)

mkProfile :: Text -> Word64 -> (Capset, [[Int]]) -> Profile
mkProfile pname interval (_n, samples) = SampledProfile sampledProfile
  where
    sampledProfile = MkSampledProfile
      { unit       = Nanoseconds
      , name       = pname
      , startValue = 0
      , endValue   = length samples
      , weights    = fromIntegral <$> sample_weights
      , samples
      }
    sample_weights = replicate (length samples) interval

parseIdent :: Text -> Maybe (Version, Text)
parseIdent s = convert $ listToMaybe $ flip readP_to_S (Data.Text.unpack s) $ do
  void $ string "GHC-"
  [v1, v2, v3] <- replicateM 3 (intP <* optional (char '.'))
  skipSpaces
  return $ makeVersion [v1,v2,v3]
  where
    intP = do
      x <- munch1 isDigit
      return $ read x

    convert x = (\(a, b) -> (a, Data.Text.pack b)) <$> x

data EL = EL {
    prog_name :: Maybe Text
  , rts_version :: Maybe (Version, Text)
  , prof_interval :: Maybe Word64
  , cost_centres :: [CostCentre]
  , el_samples :: [Sample]
}

data CostCentre = CostCentre Word32 Text Text Text deriving Show

data Sample = Sample Capset [Word32]
