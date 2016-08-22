{-# LANGUAGE OverloadedStrings #-}
module CMS.ToHtml(
	module CMS.ToHtml,
	Html
) where


import CMS.Types

import Lucid
import qualified Data.Text as T


pageToHtml :: Page -> Html ()
pageToHtml x =
	mconcat $
	map articleToHtml $
	page_content x
	{-
	textContent (article_title x) $
	mconcat $
	map sectionToHtml $
	article_content x
	-}

articleToHtml :: Article -> Html ()
articleToHtml x =
	renderArticle (article_title x) $
	mconcat $
	map sectionToHtml $
	article_content x

sectionToHtml :: Section -> Html ()
sectionToHtml x =
	renderSection (section_title x) $
		mconcat $
		map contentToHtml $
		section_content x

contentToHtml :: WebContent -> Html ()
contentToHtml x =
	case x of
		Text text ->
			p_ $ toHtml text
		Image uri ->
			img_ [src_ $ T.pack uri, alt_ "an image"]

type Title = T.Text

renderArticle :: Maybe Title -> Html () -> Html ()
renderArticle mTitle content =
	article_ [class_ "w3-panel w3-border w3-container"] $ do
		maybe mempty
			(\title -> header_ [class_ "w3-container w3-light-blue"] $ h1_ $ toHtml title)
			mTitle
		content

renderSection :: Maybe Title -> Html () -> Html ()
renderSection mTitle content =
	section_ [] $ do
		maybe
			mempty
			(\title -> header_ [class_ "w3-light-blue"] $ h2_ $ toHtml title)
			mTitle
		content
