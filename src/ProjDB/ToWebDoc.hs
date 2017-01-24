{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ProjDB.ToWebDoc(
	projectToArticle,
	artistToArticle,
	-- artistsList, -- needed?
) where

import ProjDB.Types as ProjDB
import WebDocumentStructure.Types as WebDocs

import qualified Data.Text as T
--import Data.Maybe


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

projDataToWebContent :: ProjectData -> WebContent
projDataToWebContent x =
	case x of
		ProjDB.Audio path -> WebDocs.Audio path
		ProjDB.Document DocumentInfo{..} ->
			Download $ DownloadInfo ("download " `T.append` doc_descr) doc_path

{-
artistsList :: T.Text -> ProjDB -> Page
artistsList title db =
	Page title $
	map (fromMaybe (error "internal error!") . flip artistToArticle db) $
	allArtists db
-}

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
