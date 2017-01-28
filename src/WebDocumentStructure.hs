{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module WebDocumentStructure(
	module WebDocumentStructure,
	module WebDocumentStructure.Types,
	module WebDocumentStructure.ToHtml,
) where

import WebDocumentStructure.Types
import WebDocumentStructure.ToHtml

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import System.FilePath.Posix
import Data.Yaml
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Applicative


--type ContentWithPos = (Content, URI)

{- |this type represents a hierarchy of displayable content.
	It might be used as a basis for a site navigation (menu)
-}
type Content = [ContentEntry]

data ContentEntry
	= ContentEntry {
		content_caption :: T.Text,
		content_subEntries :: Either URI [ContentEntry]
	}
	deriving( Show, Read, Eq, Ord, Generic )

loadContent :: 
	(MonadIO m, MonadError String m) =>
	FilePath -> m Content
loadContent = loadYaml

data Resource
	= PageResource Page
	| FileResource FileResInfo
	deriving( Show, Read )

data FileResInfo
	= FileResInfo {
		fileRes_type :: ResType,
		fileRes_file :: FilePath
	}
	deriving( Show, Read )

newtype URI = URI { fromURI :: FilePath }
	deriving( Eq, Ord, Show, Read, Generic )
newtype ResType = ResType { fromResType :: T.Text }
	deriving( Eq, Ord, Show, Read )

toURI :: FilePath -> URI
toURI =
	URI . normalizeURI
	where
		normalizeURI = ("/" </>)

loadYaml ::
	(FromJSON res, MonadIO m, MonadError String m) =>
	FilePath -> m res
loadYaml filename =
	do
		ma <- liftIO $ 
			either (Left . show) Right
			<$> decodeFileEither filename
		either
			(\e -> throwError $ concat ["error while loading \"", filename,"\": ", e])
			return
			ma

instance FromJSON ContentEntry where
	parseJSON (Object x) =
		ContentEntry <$>
			x .: "caption" <*> (
			(fmap Left $ x .: "uri")
			<|>
			(fmap Right $ x .: "sub")
			)
	parseJSON _ = mempty

instance FromJSON URI where
	parseJSON = (toURI <$>) . parseJSON

instance ToJSON URI where
	toJSON = toJSON . fromURI
