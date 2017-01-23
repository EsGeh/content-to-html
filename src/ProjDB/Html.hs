{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ProjDB.Html where

import ProjDB.Types as ProjDB
import CMS.Types

import qualified Data.Text as T
import Data.Maybe


{-
projectsList :: T.Text -> ProjDB -> Page
projectsList title db =
	Page title $
	map (
		fromMaybe (error "internal error!")
		. flip projectToArticle db
	) $
	allProjects db
-}

projectToArticle :: Project -> ProjDB -> Maybe Article
projectToArticle Project{..} _ =
	do
		{-
		artist <-
			catMaybes $ -- ??
			map (flip lookupArtist db) $
			project_artist project
		-}
		return $
			Article (Just $ project_name) $
			[ Section Nothing $
				map projDataToWebContent project_data
			]

projDataToWebContent x =
	case x of
		ProjDB.Audio path -> CMS.Types.Audio path
		ProjDB.Document DocumentInfo{..} ->
			Download $ DownloadInfo ("download " `T.append` doc_descr) doc_path

artistsList :: T.Text -> ProjDB -> Page
artistsList title db =
	Page title $
	map (fromMaybe (error "internal error!") . flip artistToArticle db) $
	allArtists db

artistToArticle :: ArtistKey -> ProjDB -> Maybe Article
artistToArticle key db =
	do
		artist <- lookupArtist key db
		let projects =
			projectsFromArtist (artist_name artist) db
			:: [Project]
		return $
			Article
			(Just $ artist_name artist) $ [
				Section (Just $ "projects") $ [
					Text $
					T.intercalate ", " $
					map project_name $
					projects
				]
			]

{-
toSections :: MusicList -> [Section]
toSections =
	map $ \e ->
		Section
			(Just $ T.concat [entry_artist e, ": ", entry_title e])
			[Text $ entry_comment e]

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
-}
