{-# LANGUAGE OverloadedStrings #-}
module MusicList.Html where

import MusicList.Types
import CMS.Types

import Lucid
import qualified Data.Text as T
import Data.String


toSections :: MusicList -> [Section]
toSections =
	map $ \e ->
		Section
			(Just $ T.concat [entry_artist e, ": ", entry_title e])
			[Text $ entry_comment e]

{-
{-
renderPage :: T.Text
renderPage =
	LT.toStrict $ renderText $ test
-}

showMusicList :: MusicList -> Html ()
showMusicList musicL =
	mapM_ (showEntry) musicL

showEntry :: Entry -> Html ()
showEntry entry =
	let
		artist = entry_artist entry
		title = entry_title entry
		comment = entry_comment entry
	in
		section (Just $ T.concat [artist, ": ", title]) $
			toHtml comment

{-
showMusicList :: MusicList -> Html ()
showMusicList musicL =
	ul_ $
		mapM_ ((li_ $) . showEntry) musicL

showEntry :: Entry -> Html ()
showEntry entry =
	let
		artist = entry_artist entry
		title = entry_title entry
		comment = entry_comment entry
	in
		do
			ul_ $ mapM_ (li_ . toHtml) $
				[ artist, title, comment ]
-}
-}

addEntry :: T.Text -> Html ()
addEntry action =
	form_ [action_ action, method_ "post"] $ do
		input_ [type_ "text", name_ "artist"] -- (toHtml "artist")
		input_ [type_ "text", name_ "title"]
		input_ [type_ "textarea", name_ "comment"]
		input_ [type_ "submit", value_ "submit", method_ "post"]

readAddEntryParams :: (Monad m, IsString a) => (a -> m T.Text) -> m Entry
readAddEntryParams getParam =
	do
		artist <- getParam "artist"
		title <- getParam "title"
		comment <- getParam "comment"
		return $ Entry artist title comment
