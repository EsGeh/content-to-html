{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins.HierarchicWebsite.Types where

import Types.URI
import Utils.Yaml

import qualified Data.Text as T
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Applicative


{- |this type represents a hierarchy of displayable content.
	It might be used as a basis for a site navigation (menu)
-}
type Content = [ContentEntry]

-- |an entry in the content hierarchy
data ContentEntry
	= ContentEntry {
		content_caption :: T.Text,
		content_subEntries :: Either URI [ContentEntry] -- ^ either a file or subentries
	}
	deriving( Show, Read, Eq, Ord, Generic )

loadContent :: 
	(MonadIO m, MonadError String m) =>
	FilePath -> m Content
loadContent = loadYaml

instance FromJSON ContentEntry where
	parseJSON (Object x) =
		ContentEntry <$>
			x .: "caption" <*> (
			(fmap Left $ x .: "uri")
			<|>
			(fmap Right $ x .: "sub")
			)
	parseJSON _ = mempty
