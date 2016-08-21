{-# LANGUAGE DeriveGeneric #-}
module MusicList.Types where

import Data.Yaml
import Control.Concurrent.STM
import GHC.Generics


data MusicListState
	= MusicListState {
		mlState_ml :: TVar MusicList
	}

type MusicList = [Entry]

data Entry
	= Entry {
		entry_artist :: String,
		entry_title :: String,
		entry_comment :: String
	}
	deriving( Read, Show, Generic, Eq, Ord )

type Key = (String, String) -- Artist, Title

instance FromJSON Entry
instance ToJSON Entry
-- toEncoding = genericToEncoding defaultOptions
