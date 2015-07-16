{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GitToggl.Types
  where

import Data.Aeson
import Data.Hourglass (DateTime, ISO8601_DateAndTime(..), LocalTime(..), localTimePrint)
import Data.Text (Text)
import qualified Data.Text as Text (lines)

data Options = Init (Maybe FilePath)
             | Clean (Maybe FilePath)
             | PrepareCommitMsg FilePath
             | CommitMsg FilePath
             | Login

data TogglAuth = Token String

data Commit = Commit { commitStart :: LocalTime DateTime
                     , commitStop :: LocalTime DateTime
                     , commitSha :: Text
                     , commitMessage :: Text
                     , commitRepository :: Maybe Repository
                     , commitAuthor :: Text
                     }

instance ToJSON Commit where
    toJSON Commit{..} = o [ "time_entry" .=
                            o [ "start" .= localTimePrint ISO8601_DateAndTime commitStart
                              , "stop" .= localTimePrint ISO8601_DateAndTime commitStop
                              , "description" .= head (Text.lines commitMessage)
                              , "project" .= commitRepository
                              ]
                          , "metadata" .=
                            o [ "commit" .= commitSha
                              , "repository" .= commitRepository
                              ]
                          ]
      where
        o = object

data Repository = GithubRepository String

instance ToJSON Repository where
    toJSON (GithubRepository repo) = toJSON repo

data TogglTimeFormat = Improved
                     | Classic
                     | Decimal
