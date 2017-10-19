{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Types.WebDocument where

import Utils.JSONOptions
import Types.URI
import Utils.Yaml

import qualified Data.Text as T
import Data.Aeson.TH
import qualified Data.Map as M
import GHC.Generics
import Control.Monad.Identity
import Data.Maybe


-- | hierarchical html-document-like structure
data PageWithNav
	= PageWithNav {
		pageWithNav_nav :: Nav,
		pageWithNav_page :: Section,
		pageWithNav_headerInfo :: HeaderInfo
	}
	deriving( Show, Read, Eq, Ord, Generic )

type Nav = [NavEntry]

data HeaderInfo
	= HeaderInfo {
		headerInfo_userCss :: [URI],
		headerInfo_addText :: T.Text
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

type Attributes = M.Map T.Text T.Text

attributes_empty :: Attributes
attributes_empty = M.empty

attributes_join :: Attributes -> Attributes -> Attributes
attributes_join = M.unionWith $ \x y -> T.intercalate " " [x, y]

-- |a section in the document
type Section = SectionGen SectionInfo
-- |a tree of sections in which the leafs can optionally be "variables" of type var
type SectionTemplate var = SectionGen (Either var SectionInfo)

-- |A node in a tree of sections, either primitive or complex...
-- The type parameter contains the information describing the leaf nodes
data SectionGen sectionInfo
	= SectionEntry sectionInfo -- ^ primitive (leaf)
	| SectionNode (SectionNodeInfo sectionInfo) -- ^ complex section (inner node)
	deriving( Show, Read, Eq, Ord, Generic )

-- |a complex section consisting of other sections (inner node in the doc tree)
type SectionNodeInfo sectionInfo = SectionInfoGen [SectionGen sectionInfo]
-- |a primitive section (leaf of the doc tree)
type SectionInfo = SectionInfoGen WebContent

-- information for a "primitive" section containing additional info of type 'content'
data SectionInfoGen content
	= SectionInfo {
		section_title :: Maybe Title,
		section_content :: content,
		section_attributes :: Attributes
	}
	deriving( Show, Read, Eq, Ord, Generic )

defSectionInfo :: content -> SectionInfoGen content
defSectionInfo content = SectionInfo Nothing content attributes_empty

instance Functor SectionGen where
	fmap f = \case
		SectionEntry x -> SectionEntry $ f x
		SectionNode l -> SectionNode $ sectionInfo_mapToContent (map $ fmap f) l

instance FromJSON SectionInfo where
	parseJSON = withObject "section info" $ \o ->
		do
			title <- o .:? "title"
			content <- o .: "content"
			props <-
				(fromMaybe attributes_empty) <$> o .:? "attributes"
			return $ SectionInfo title content props
			--style <- (StyleInfo) <$> o .:? "style_class"

sectionInfo_mapToContentM ::
	Monad m =>
	(content -> m content') -> SectionInfoGen content -> m (SectionInfoGen content')
sectionInfo_mapToContentM f p@SectionInfo{..} =
	f section_content >>= \new ->
	return p{ section_content = new }

sectionInfo_mapToContent :: 
	(content -> content') -> SectionInfoGen content -> SectionInfoGen content'
sectionInfo_mapToContent f = runIdentity . sectionInfo_mapToContentM (return . f)

section :: content -> SectionGen (SectionInfoGen content)
section content =
	SectionEntry $ defSectionInfo content
sectionWithTitle :: T.Text -> content -> SectionGen (SectionInfoGen content)
sectionWithTitle title content =
	SectionEntry $ (defSectionInfo content){ section_title = Just title }

mainSection :: [SectionGen info] -> SectionGen info
mainSection content =
	SectionNode $ defSectionInfo content
mainSectionWithTitle :: T.Text -> [SectionGen info] -> SectionGen info
mainSectionWithTitle title content =
	SectionNode $ (defSectionInfo content){ section_title = Just title }

eitherSection ::
	(info -> b)
	-> (SectionNodeInfo info -> b)
	-> SectionGen info -> b
eitherSection l r = \case
	SectionEntry e -> l e
	SectionNode e -> r e

class HasTitle a where
	sectionTitle :: a -> Maybe Title

instance HasTitle (SectionInfoGen content) where
	sectionTitle = section_title

instance HasTitle (SectionGen (SectionInfoGen content)) where
	sectionTitle (SectionEntry e) = sectionTitle e
	sectionTitle (SectionNode e) = sectionTitle e

class HasAttributes a where
	getAttributes :: a -> Attributes

instance HasAttributes (SectionInfoGen content) where
	getAttributes = section_attributes

instance HasAttributes (SectionGen (SectionInfoGen content)) where
	getAttributes (SectionEntry e) = getAttributes e
	getAttributes (SectionNode e) = getAttributes e

data WebContent
	= Text T.Text
	| Image URI
	| Audio URI
	| Download DownloadInfo
	| Form FormInfo
	deriving( Show, Read, Eq, Ord, Generic  )

data FormInfo
	= FormInfo {
		form_content :: [FormEntry],
		form_action :: T.Text,
		form_method :: FormMethod
	}
	deriving( Show, Read, Eq, Ord, Generic  )

data FormMethod
	= Get
	| Post
	deriving( Eq, Ord, Generic  )

instance Show FormMethod where
	show Get = "get"
	show Post = "post"

instance Read FormMethod where
	readsPrec = \case
		{-
		"get" -> Get
		"post" -> Post
		-}
		_ -> error "read FormMethod error"

data FormEntry
	= FormEntry {
		formEntry_caption :: T.Text,
		formEntry_type :: FormEntryType,
		formEntry_name :: T.Text,
		formEntry_default :: T.Text
	}
	deriving( Show, Read, Eq, Ord, Generic  )

formEntryTypeToText :: FormEntryType -> T.Text
formEntryTypeToText = \case
	TextInput -> T.pack "text"
	SubmitInput -> T.pack "submit"

data FormEntryType
	= TextInput
	| TextAreaInput
	| SubmitInput
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
-- $(deriveJSON jsonOptions ''Article)
-- $(deriveJSON jsonOptions ''Section)
$(deriveJSON jsonOptions ''WebContent)
$(deriveJSON jsonOptions ''FormInfo)
$(deriveJSON jsonOptions ''FormMethod)
$(deriveJSON jsonOptions ''FormEntry)
$(deriveJSON jsonOptions ''FormEntryType)
