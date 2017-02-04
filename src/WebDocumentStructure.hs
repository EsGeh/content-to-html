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
import Types

import Data.Aeson
import GHC.Generics
import Data.Yaml
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Applicative
import qualified Data.Text as T


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

type ResourceTemplate = ResourceGen (SectionTemplate Request)

type Resource = ResourceGen Section

data ResourceGen section
	= FullPageResource PageWithNav
	| PageResource section
	| FileResource FileResInfo
	deriving( Show, Read )

{-
type Resource = ResourceGen Page
type ResourceTemplate = ResourceGen (PageTemplate Request)

data ResourceGen page
	= FullPageResource PageWithNav
	| PageResource page
	| FileResource FileResInfo
	deriving( Show, Read )
-}

data FileResInfo
	= FileResInfo {
		fileRes_type :: ResType,
		fileRes_file :: FilePath
	}
	deriving( Show, Read )

type Request = (URI, Params)
type Params = [(T.Text, T.Text)]
-- type Params = M.Map T.Text T.Text

newtype ResType = ResType { fromResType :: T.Text }
	deriving( Eq, Ord, Show, Read )

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
