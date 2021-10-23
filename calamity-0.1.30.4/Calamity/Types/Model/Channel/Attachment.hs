-- | Message attachments
module Calamity.Types.Model.Channel.Attachment
    ( Attachment(..) ) where

import           Calamity.Internal.AesonThings ()
import           Calamity.Types.Snowflake
import           Data.Aeson
import           Data.Text.Lazy                ( Text )
import           Data.Word
import           GHC.Generics
import           TextShow
import qualified TextShow.Generic              as TSG

fuseTup2 :: Monad f => (f a, f b) -> f (a, b)
fuseTup2 (a, b) = do
  !a' <- a
  !b' <- b
  pure (a', b')

data Attachment = Attachment
  { id         :: Snowflake Attachment
  , filename   :: !Text
  , size       :: Word64
  , url        :: !Text
  , proxyUrl   :: !Text
  , dimensions :: !(Maybe (Word64, Word64))
  }
  deriving ( Eq, Show, Generic )
  deriving ( TextShow ) via TSG.FromGeneric Attachment
  deriving ( HasID Attachment ) via HasIDField "id" Attachment

instance ToJSON Attachment where
  toJSON Attachment { id, filename, size, url, proxyUrl, dimensions = Just (width, height) } = object
    [ "id" .= id
    , "filename" .= filename
    , "size" .= size
    , "url" .= url
    , "proxy_url" .= proxyUrl
    , "width" .= width
    , "height" .= height]
  toJSON Attachment { id, filename, size, url, proxyUrl } =
    object ["id" .= id, "filename" .= filename, "size" .= size, "url" .= url, "proxy_url" .= proxyUrl]

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \v -> do
    width <- v .:? "width"
    height <- v .:? "height"

    Attachment <$> v .: "id" <*> v .: "filename" <*> v .: "size" <*> v .: "url" <*> v .: "proxy_url" <*> pure
      (fuseTup2 (width, height))
