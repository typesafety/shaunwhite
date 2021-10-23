-- | A DM channel with a single person
module Calamity.Types.Model.Channel.DM (DMChannel (..)) where

import Calamity.Internal.AesonThings
import Calamity.Internal.OverriddenVia
import Calamity.Internal.Utils
import {-# SOURCE #-} Calamity.Types.Model.Channel
import {-# SOURCE #-} Calamity.Types.Model.Channel.Message
import Calamity.Types.Model.User
import Calamity.Types.Snowflake
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Time
import Data.Vector.Unboxing (Vector)
import GHC.Generics
import TextShow
import qualified TextShow.Generic as TSG

data DMChannel' = DMChannel'
  { id :: Snowflake DMChannel
  , lastMessageID :: Maybe (Snowflake Message)
  , lastPinTimestamp :: Maybe (CalamityFromStringShow UTCTime)
  , recipients :: AesonVector (Snowflake User)
  }
  deriving (Generic)
  deriving (TextShow) via TSG.FromGeneric DMChannel'
  deriving
    (FromJSON)
    via WithSpecialCases
          '["recipients" `ExtractArrayField` "id"]
          DMChannel'

data DMChannel = DMChannel
  { id :: Snowflake DMChannel
  , lastMessageID :: Maybe (Snowflake Message)
  , lastPinTimestamp :: Maybe UTCTime
  , recipients :: Vector (Snowflake User)
  }
  deriving (Show, Eq, Generic, NFData)
  deriving (TextShow, FromJSON) via OverriddenVia DMChannel DMChannel'
  deriving (HasID DMChannel) via HasIDField "id" DMChannel
  deriving (HasID Channel) via HasIDFieldCoerce' "id" DMChannel
