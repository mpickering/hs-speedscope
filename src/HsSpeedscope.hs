{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}

module HsSpeedscope (
    entry,
    convertToSpeedscope,
    processEventsDefault,
    isInfoEvent,
    parseIdent,
    EventLogProfile(..),
    CostCentre(..),
    Sample(..)
) where

import Data.String ( fromString )
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Functor.Identity (Identity (..))
import Data.List.Extra
import Data.Machine (Moore (..), source, (~>), ProcessT, PlanT, Is, construct, await, yield)
import Data.Machine.Runner (foldlT)
import Data.Maybe
import qualified Data.Text
import Data.Text (Text)
import qualified Data.Vector.Unboxed as V
import Data.Version
import Data.Word
import GHC.RTS.Events hiding (header, str)
import qualified Options.Applicative as O
import Options.Applicative hiding (optional)
import Speedscope.Schema
import Text.ParserCombinators.ReadP hiding (between)


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
  encodeFile (file ++ ".json") (convertToSpeedscope (isolateStart, isolateEnd) isInfoEvent processEventsDefault el)

-- | A Moore machine whose state indicates which delimiting markers have been
-- seen. If both markers are 'Nothing', then the state will always be 'True'.
-- If the first marker is given, then the state will always be 'False' until a
-- value which the marker is a prefix of is seen. If the second marker is given,
-- then the state will always be 'False' after a value which the marker is a
-- prefix of is seen.
markers :: (Maybe Text, Maybe Text) -> Moore Text Bool
markers (Nothing, Nothing) =
    go
  where
    go = Moore True (const go)
markers (Just s,  Nothing) =
    wait_for_start
  where
    wait_for_start =
      Moore False $ \s' ->
        if s `Data.Text.isPrefixOf` s' then
          go
        else
          wait_for_start
    go = Moore True (const go)
markers (Nothing, Just e) =
    go_until
  where
    go_until =
      Moore True $ \e' ->
        if e `Data.Text.isPrefixOf` e' then
          stop
        else
          go_until
    stop = Moore False (const stop)
markers (Just s, Just e) =
    go_between
  where
    go_between =
        Moore False wait_for_start
      where
        wait_for_start s' = if s `Data.Text.isPrefixOf` s' then go_until else go_between
    go_until =
        Moore True close'
      where
        close' e' = if e `Data.Text.isPrefixOf` e' then stop else go_until
    stop = Moore False (const stop)

-- | Delimit the event process, and only include events which satisfy a
-- predicate.
delimit
    :: Monad m
    => (EventInfo -> Bool)
    -- ^ Only emit events which pass this predicate
    -> Moore Text Bool
    -- ^ Only emit events when this 'Moore' process state is 'True'. The process
    -- will be given the values of 'UserMarker's in the event log.
    -> ProcessT m Event Event
delimit p =
    construct . go
  where
    go :: Monad m => Moore Text Bool -> PlanT (Is Event) Event m ()
    go mm@(Moore s next) = do
      e <- await
      case evSpec e of
        -- on marker step the moore machine.
        UserMarker m -> do
            let mm'@(Moore s' _) = next m
            -- if current or next state is open (== True), emit the marker.
            when (s || s') $ yield e
            go mm'

        -- for other events, emit if the state is open and predicate passes
        ei -> do
            when (s || p ei) $ yield e
            go mm

-- | Convert an 'EventLog' into a speedscope profile JSON value. To convert the
-- event log using the traditional profile extraction logic, use `isInfoEvent`
-- and `processEventsDefault` for the predicate and processing function,
-- respectively.
convertToSpeedscope
  :: (Maybe Text, Maybe Text)
  -- ^ Delimiting markers. No events before a user marker containing the first
  -- string will be included. No events after a user marker containing the
  -- second string will be included.
  -> (EventInfo -> Bool)
  -- ^ Only consider events which satisfy this predicate
  -> (EventLogProfile -> Event -> EventLogProfile)
  -- ^ Specifies how to build the profile given the events included based on the
  -- delimiters and predicate
  -> EventLog
  -> Value
convertToSpeedscope (is, ie) considerEvent processEvents (EventLog _h (Data (sortOn evTime -> es))) =
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
    Identity (EventLogProfile (fromMaybe "" -> profile_name) el_version (fromMaybe 1 -> interval) frames samples) =
        foldlT processEvents initEL $
            source es ~>
            delimit considerEvent (markers (is, ie))

    initEL = EventLogProfile Nothing Nothing Nothing [] []


    version_string :: String
    version_string = "hs-speedscope@" ++ CURRENT_PACKAGE_VERSION

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

-- | Default processing function to convert profiling events into a classic speedscope
-- profile
processEventsDefault :: EventLogProfile -> Event -> EventLogProfile
processEventsDefault elProf (Event _t ei _c) =
  case ei of
    ProgramArgs _ (pname: _args) ->
      elProf { prog_name = Just pname }
    RtsIdentifier _ rts_ident ->
      elProf { rts_version = parseIdent rts_ident }
    ProfBegin ival ->
      elProf { prof_interval = Just ival }
    HeapProfCostCentre n l m s _ ->
      elProf { cost_centres = CostCentre n l m s : cost_centres elProf }
    ProfSampleCostCentre t _ _ st ->
      elProf { el_samples = Sample (fromIntegral t) (V.toList st) : el_samples elProf }
    _ ->
      elProf

isInfoEvent :: EventInfo -> Bool
isInfoEvent ProgramArgs {}        = True
isInfoEvent RtsIdentifier {}      = True
isInfoEvent ProfBegin {}          = True
isInfoEvent HeapProfCostCentre {} = True
isInfoEvent _ = False

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

-- | The type we wish to convert event logs into
data EventLogProfile = EventLogProfile
    { prog_name :: Maybe Text
    , rts_version :: Maybe (Version, Text)
    , prof_interval :: Maybe Word64
    , cost_centres :: [CostCentre]
    , el_samples :: [Sample]
    }

data CostCentre = CostCentre Word32 Text Text Text deriving Show

data Sample = Sample Capset [Word32]
