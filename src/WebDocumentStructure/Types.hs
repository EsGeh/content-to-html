{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module WebDocumentStructure.Types where

import WebDocumentStructure.JSONOptions
import Types

import qualified Data.Text as T
import Data.Aeson.TH
import Data.Aeson
import GHC.Generics
import Control.Monad.Identity


data PageWithNav
	= PageWithNav {
		pageWithNav_nav :: Nav,
		pageWithNav_page :: Page,
		pageWithNav_headerInfo :: HeaderInfo
	}
	deriving( Show, Read, Eq, Ord, Generic )

type Nav = [NavEntry]

data HeaderInfo
	= HeaderInfo {
		headerInfo_userCss :: Maybe URI
	}
	deriving( Show, Read, Eq, Ord, Generic )

data NavEntry
	= NavEntry Link
	| NavCategory Title [NavEntry]
	deriving( Show, Read, Eq, Ord, Generic )

data Link
	= Link {
		link_caption :: Title,
		link_dest :: URI
	}
	deriving( Show, Read, Eq, Ord, Generic )

type Page = PageGen Article
type PageTemplate article = PageGen (Either article Article)

data PageGen article
	= Page {
		page_title :: Title,
		page_content :: [article]
	}
	deriving( Show, Read, Eq, Ord, Generic )

page_mapToContentM f p@Page{..} =
	f page_content >>= \new ->
	return p{ page_content = new }

page_mapToContent f = runIdentity . page_mapToContentM (return . f)

instance FromJSON Page where
--instance FromJSON (PageTemplate Request) where

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
	| Image URI
	| Audio URI
	| Download DownloadInfo
	deriving( Show, Read, Eq, Ord, Generic  )

data DownloadInfo
	= DownloadInfo {
		download_caption :: T.Text,
		download_uri :: URI
	}
	deriving( Show, Read, Eq, Ord, Generic  )

type Title = T.Text

instance FromJSON DownloadInfo where
	parseJSON (Object x) =
		DownloadInfo <$>
		x.: "caption" <*>
		x.: "uri"
	parseJSON _ = mempty

instance ToJSON DownloadInfo where
	toJSON DownloadInfo{..} = object $
		[ "caption" .= download_caption
		, "uri" .= download_uri
		]

-- $(deriveJSON jsonOptions ''Page)
$(deriveJSON jsonOptions ''Article)
$(deriveJSON jsonOptions ''Section)
$(deriveJSON jsonOptions ''WebContent)
