{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE
      DeriveGeneric
    , DeriveDataTypeable
    , TemplateHaskell
    , TypeFamilies
    , EmptyDataDecls
    #-}
module Api.Item (resource) where

import Data.Aeson
import Rest
import Rest.Resource as R

import Data.Map (Map(..))
import qualified Data.Map as M

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.IO.Class (liftIO)

import Data.Typeable
import GHC.Generics (Generic(..))

import Generics.Regular
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

import Data.JSON.Schema (JSONSchema(..))

data IId = IId Int
  deriving (Eq,Ord,Show,Generic,Typeable)

deriveAll ''IId "PFIId"
type instance PF IId = PFIId

instance ToJSON IId
instance FromJSON IId
instance JSONSchema IId
instance XmlPickler IId where xpickle = gxpickle

data Item = Item { iId :: !IId
                 , content :: String
                 } deriving (Eq,Show,Generic,Typeable)

deriveAll ''Item "PFItem"
type instance PF Item = PFItem

instance ToJSON Item
instance FromJSON Item
instance JSONSchema Item
instance XmlPickler Item where xpickle = gxpickle

resource :: Resource IO (ReaderT IId IO) IId () Void
resource = mkResourceReader
  { R.name   = "item"
  , R.schema = withListing () $ named [("single", singleRead IId)]
  , R.get    = Just getItem
  , R.list   = const listI
  }

listI :: ListHandler IO
listI = mkListing xmlJsonO $ \rge -> liftIO $ listItems (offset rge) (count rge)

listItems :: Int -> Int -> IO [Item]
listItems s n = return . take n  $ M.elems dataset

getItem :: Handler (ReaderT IId IO)
getItem = mkIdHandler xmlJsonO $ \_ iid -> liftIO $ getItemById iid

getItemById :: IId -> IO (Maybe Item)
getItemById i = return $ M.lookup i dataset

dataset :: Map IId Item
dataset = M.fromList [(IId 11, Item { iId = IId 11, content = "steeevveee" })
                     ,(IId 22, Item { iId = IId 22, content = "celebrate!" })
                     ]
