{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Html where

import WebDocumentStructure

import Lucid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Monoid


renderPage :: Html () -> T.Text
renderPage =
	LT.toStrict . renderText

basePage :: Maybe URI -> Title -> Html () -> Html ()
basePage mUserCss title content =
	html_ $ do
		head_ $ do
			meta_ [charset_ "UTF-8"]
			link_ [rel_ "stylesheet", href_ "http://www.w3schools.com/lib/w3.css"]
			maybe (return ()) `flip` mUserCss $ \userCss ->
				link_ [rel_ "stylesheet", href_ (T.pack $ fromURI userCss)]
			title_ $ toHtml title
		body_ $
			content

nav :: Content -> Html ()
nav = nav_ . nav'

nav' :: Content -> Html ()
nav' content =
	ul_ [class_ "w3-navbar w3-border w3-light-blue"] $ mconcat $
		map `flip` content $ \e ->
			li_ $
			case content_subEntries e of
				Left uri ->
					link uri (content_caption e)
				Right sub ->
					toHtml (content_caption e) <> nav' sub

link :: URI -> T.Text -> Html ()
link route name =
	a_ [href_ $ T.pack (fromURI route)] (toHtml name)
