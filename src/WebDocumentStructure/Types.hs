{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module WebDocumentStructure.Types where

import WebDocumentStructure.JSONOptions

import qualified Data.Text as T
import Data.Aeson.TH
import Data.Aeson
import GHC.Generics


data PageWithNav
	= PageWithNav {
		pageWithNav_nav :: Nav,
		pageWithNav_page :: Page
	}
	deriving( Show, Read, Eq, Ord, Generic )

type Nav = [NavEntry]

data NavEntry
	= NavEntry Link
	| NavCategory Title [NavEntry]
	deriving( Show, Read, Eq, Ord, Generic )

data Link
	= Link {
		link_caption :: Title,
		link_dest :: T.Text
	}
	deriving( Show, Read, Eq, Ord, Generic )

data Page
	= Page {
		page_title :: Title,
		page_content :: [Article]
	}
	deriving( Show, Read, Eq, Ord, Generic )

data Article
	= Article {
		article_title :: Maybe Title,
		article_content :: [Section]
	}
	deriving( Show, Read, Eq, Ord, Generic )

data Section = Section {
	section_title :: Maybe Title,
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

type Title = T.Text

instance FromJSON DownloadInfo where
	parseJSON (Object x) =
		DownloadInfo <$>
		x.: "caption" <*>
		x.: "path"
	parseJSON _ = mempty

instance ToJSON DownloadInfo where
	toJSON DownloadInfo{..} = object $
		[ "caption" .= download_caption
		, "path" .= download_filename
		]

$(deriveJSON jsonOptions ''Page)
$(deriveJSON jsonOptions ''Article)
$(deriveJSON jsonOptions ''Section)
$(deriveJSON jsonOptions ''WebContent)
