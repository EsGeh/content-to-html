{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Types.WebDocument.ToHtml(
	module Types.WebDocument.ToHtml,
	Html
) where

import Types.WebDocument
import Types.URI

import Lucid
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe


pageWithNavToHtml :: PageWithNav -> Html ()
pageWithNavToHtml PageWithNav{..} =
	htmlHeader pageWithNav_headerInfo (fromMaybe "untitled" $ sectionTitle pageWithNav_page) $
		(div_ [class_ "menu-section"] $ navToHtml pageWithNav_nav)
		<>
		(div_ [class_ "main-section"] $ sectionToHtml pageWithNav_page)

htmlHeader :: HeaderInfo -> Title -> Html () -> Html ()
htmlHeader HeaderInfo{..} title content =
	html_ $ do
		meta_ [charset_ "UTF-8"]
		head_ $ do
			-- link_ [rel_ "stylesheet", href_ "http://www.w3schools.com/lib/w3.css"]
			maybe (return ()) `flip` headerInfo_userCss $ \userCss ->
				link_ [rel_ "stylesheet", href_ (T.pack $ fromURI userCss)]
			title_ $ toHtml title
		body_ $ content

navToHtml :: Nav -> Html ()
navToHtml =
	navToHtml' (0 :: Int)
	where
		navToHtml' depth nav =
			ul_ [class_ $ T.pack $ "menu " ++ depthAttribute depth] $ mconcat $ map `flip` nav $ \case
				NavEntry link ->
					li_ [class_ $ T.pack $ "menuentry " ++ depthAttribute depth] $
					linkToHtml link
				NavCategory title subEntries ->
					li_ [class_ $ T.pack $ "menucategory " ++ depthAttribute depth] $
					toHtml title <> navToHtml' (depth+1) subEntries

linkToHtml :: Link -> Html ()
linkToHtml Link{..} =
	a_ [href_ . T.pack . fromURI $ link_dest] $ toHtml $ T.unpack link_caption

sectionToHtml :: Section -> Html ()
sectionToHtml = sectionToHtml' 0
	where
		sectionToHtml' depth x =
			div_ (sectionAttributes depth x) $
			renderSection depth (sectionTitle x) $
			eitherSection (contentToHtml . section_content) `flip` x $
			(mconcat . map (sectionToHtml' $ depth+1) . section_content)

sectionAttributes :: Int -> Section -> [Attribute]
sectionAttributes depth x =
	[class_ $ T.intercalate " " $
		maybe id (\y -> ([y] ++)) (style_class $ sectionStyle x) $
		[ "section"
		, T.pack $ depthAttribute depth 
		]
	]

depthAttribute :: Int -> String
depthAttribute depth =
	"depth-" ++ (show depth)

{-
mapWithCtxt f x = mapWithCtxt' 0 x
	where
		mapWithCtxt' depth x =
			case x of
				SectionEntry info -> SectionEntry $ f depth x
				MainSection l ->
					MainSection $ sectionInfo_mapToContent (map $ mapWithCtxt' (depth+1)) l
-}

{-
foldSection :: (a -> [Section] -> a) -> Section -> a
foldSection f x =
	renderSection (sectionTitle x) $
	eitherSection (contentToHtml . section_content) `flip` x $
	(mconcat . map sectionToHtml' . section_content)
-}

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

renderSection :: Int -> Maybe Title -> Html () -> Html ()
renderSection _ mTitle content =
	do
	--section_ [] $ do
		maybe
			mempty
			(\title -> header_ headerClass $ h2_ $ toHtml title)
			mTitle
		content
