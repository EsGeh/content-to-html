{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module CMS.Types where

import CMS.JSONOptions

import qualified Data.Text as T
import Data.Aeson.TH
import Data.Aeson
import GHC.Generics


data Page
	= Page {
		page_title :: T.Text,
		page_content :: [Article]
	}
	deriving( Show, Read, Eq, Ord, Generic )

data Article
	= Article {
		article_title :: Maybe T.Text,
		article_content :: [Section]
	}
	deriving( Show, Read, Eq, Ord, Generic )

data Section = Section {
	section_title :: Maybe T.Text,
	section_content:: [WebContent]
}
	deriving( Show, Read, Eq, Ord, Generic )

data WebContent
	= Text T.Text
	| Image FilePath
	| Audio FilePath
	| Download DownloadInfo
	deriving( Show, Read, Eq, Ord, Generic  )

data DownloadInfo
	= DownloadInfo {
		download_caption :: T.Text,
		download_filename :: FilePath
	}
	deriving( Show, Read, Eq, Ord, Generic  )

instance FromJSON DownloadInfo where
	parseJSON (Object x) =
		DownloadInfo <$>
		x.: "caption" <*>
		x.: "path"

instance ToJSON DownloadInfo where
	toJSON DownloadInfo{..} = object $
		[ "caption" .= download_caption
		, "path" .= download_filename
		]

$(deriveJSON jsonOptions ''Page)
$(deriveJSON jsonOptions ''Article)
$(deriveJSON jsonOptions ''Section)
$(deriveJSON jsonOptions ''WebContent)

{-
instance ToJSON WebContent
instance FromJSON WebContent
-}
