-- | Permission overwrites
module Calamity.Types.Model.Guild.Overwrite (Overwrite (..)) where

import Calamity.Internal.AesonThings
import Calamity.Types.Model.Guild.Permissions
import Calamity.Types.Snowflake

import Data.Aeson

import GHC.Generics

import Control.DeepSeq (NFData)
import TextShow
import qualified TextShow.Generic as TSG

data Overwrite = Overwrite
  { id :: Snowflake Overwrite
  , type_ :: Int
  , allow :: Permissions
  , deny :: Permissions
  }
  deriving (Eq, Show, Generic, NFData)
  deriving (TextShow) via TSG.FromGeneric Overwrite
  deriving (ToJSON, FromJSON) via CalamityJSON Overwrite
  deriving (HasID Overwrite) via HasIDField "id" Overwrite
