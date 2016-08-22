{-# LANGUAGE DeriveGeneric #-}
module MusicList.Types where

import Data.Yaml
import Control.Concurrent.STM
import GHC.Generics
import qualified Data.Text as T


data MusicListState
	= MusicListState {
		mlState_ml :: TVar MusicList
	}

type MusicList = [Entry]

data Entry
	= Entry {
		entry_artist :: T.Text,
		entry_title :: T.Text,
		entry_comment :: T.Text
	}
	deriving( Read, Show, Generic, Eq, Ord )

type Key = (T.Text, T.Text) -- Artist, Title

instance FromJSON Entry
instance ToJSON Entry
-- toEncoding = genericToEncoding defaultOptions
