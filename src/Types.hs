{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Aeson
import System.FilePath.Posix
import GHC.Generics


newtype URI = URI { fromURI :: FilePath }
	deriving( Eq, Ord, Show, Read, Generic )

toURI :: FilePath -> URI
toURI =
	URI . normalizeURI
	where
		normalizeURI = ("/" </>)

instance FromJSON URI where
	parseJSON = (toURI <$>) . parseJSON

instance ToJSON URI where
	toJSON = toJSON . fromURI
