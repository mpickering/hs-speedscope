{-# language DeriveAnyClass #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}

module Speedscope.Schema
  ( File(..)
  , Shared(..)
  , Frame(..)
  , Profile(..)
  , SampledProfile(..)
  , Unit(..)
  ) where

import Data.Char ( toLower )
import Data.Aeson ( ToJSON(..), object, (.=), Value( String ) )
import GHC.Generics ( Generic )
import Data.Text ( Text )


data File = File
  { activeProfileIndex :: Maybe Int
  , exporter           :: Maybe Text
  , name               :: Maybe Text
  , profiles           :: [Profile]
  , shared             :: Shared
  }


instance ToJSON File where
  toJSON File{..} = object
    [ "$schema" .= String "https://www.speedscope.app/file-format-schema.json"
    , "activeProfileIndex" .= activeProfileIndex
    , "exporter" .= exporter
    , "name" .= name
    , "profiles" .= profiles
    , "shared" .= shared
    ]


newtype Shared = Shared
  { frames :: [Frame]
  } deriving ( Generic, ToJSON )


data Frame = Frame
  { col  :: Maybe Int
  , file :: Maybe Text
  , line :: Maybe Int
  , name :: Text
  } deriving ( Generic, ToJSON )


data Profile = SampledProfile SampledProfile


instance ToJSON Profile where
  toJSON = \case
    SampledProfile p -> toJSON p


data SampledProfile = MkSampledProfile
  { endValue   :: Int
  , name       :: Text
  , samples    :: [[Int]]
  , startValue :: Int
  , unit       :: Unit
  , weights    :: [Int]
  }


instance ToJSON SampledProfile where
  toJSON MkSampledProfile{..} = object
    [ "endValue"   .= endValue
    , "name"       .= name
    , "samples"    .= samples
    , "startValue" .= startValue
    , "unit"       .= unit
    , "weights"    .= weights
    , "type"       .= String "sampled"
    ]


data Unit = Nanoseconds
  deriving Show


instance ToJSON Unit where
  toJSON = toJSON . map toLower . show
