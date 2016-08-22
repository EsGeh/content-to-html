{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module CMS.Types where

import CMS.JSONOptions

import qualified Data.Text as T
import Data.Yaml
import Data.Aeson.TH
import GHC.Generics


{-
type Entry = Either Subsection Page

data SubEntry
	= SubEntry {
		subEntry_title :: T.Text,
		subEntry_title :: [Entry]
	}
-}

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
	deriving( Show, Read, Eq, Ord, Generic  )


$(deriveJSON jsonOptions ''Page)
$(deriveJSON jsonOptions ''Article)
$(deriveJSON jsonOptions ''Section)
$(deriveJSON jsonOptions ''WebContent)

{-
instance ToJSON WebContent
instance FromJSON WebContent
-}
