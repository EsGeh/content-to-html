{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module WebDocumentStructure.ToHtml(
	module WebDocumentStructure.ToHtml,
	Html
) where

import WebDocumentStructure.Types
import Types

import Lucid
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe


pageWithNavToHtml :: PageWithNav -> Html ()
pageWithNavToHtml PageWithNav{..} =
	htmlHeader pageWithNav_headerInfo (fromMaybe "untitled" $ sectionTitle pageWithNav_page) $
		navToHtml pageWithNav_nav
		<>
		sectionToHtml pageWithNav_page

htmlHeader :: HeaderInfo -> Title -> Html () -> Html ()
htmlHeader HeaderInfo{..} title content =
	html_ $ do
		head_ $ do
			meta_ [charset_ "UTF-8"]
			link_ [rel_ "stylesheet", href_ "http://www.w3schools.com/lib/w3.css"]
			maybe (return ()) `flip` headerInfo_userCss $ \userCss ->
				link_ [rel_ "stylesheet", href_ (T.pack $ fromURI userCss)]
			title_ $ toHtml title
		body_ $ content

navToHtml :: Nav -> Html ()
navToHtml nav =
	ul_ [] $ mconcat $ map `flip` nav $ li_ . \case
		NavEntry link -> linkToHtml link
		NavCategory title subEntries ->
			toHtml title <> navToHtml subEntries
	--mconcat $ map linkToHtml nav

linkToHtml :: Link -> Html ()
linkToHtml Link{..} =
	a_ [href_ . T.pack . fromURI $ link_dest] $ toHtml $ T.unpack link_caption

sectionToHtml :: Section -> Html ()
sectionToHtml x =
	renderSection (sectionTitle x) $
	eitherSection (contentToHtml . section_content) `flip` x $
	(mconcat . map sectionToHtml . section_content)

contentToHtml :: WebContent -> Html ()
contentToHtml x =
	case x of
		Text text ->
			p_ $ toHtml text
		Image uri ->
			img_ [src_ $ T.pack . fromURI $ uri, alt_ "an image"]
		Audio uri ->
			p_ $
			audio_ [controls_ "hussa"] $ do
				source_ [src_ . T.pack . fromURI $ uri]
				toHtml $ T.pack "your browser seems not to support html5 audio playback"
		Download DownloadInfo{..} ->
			a_ [href_ . T.pack . fromURI $ download_uri, download_ "" ] $ toHtml $ T.unpack download_caption

headerClass :: [Attribute]
headerClass =
	[]
	--[class_ "w3-container w3-light-blue"]

renderArticle :: Maybe Title -> Html () -> Html ()
renderArticle mTitle content =
	article_ [class_ "w3-panel w3-border w3-container"] $ do
		maybe mempty
			(\title -> header_ headerClass $ h1_ $ toHtml title)
			mTitle
		content

renderSection :: Maybe Title -> Html () -> Html ()
renderSection mTitle content =
	section_ [] $ do
		maybe
			mempty
			(\title -> header_ headerClass $ h2_ $ toHtml title)
			mTitle
		content
