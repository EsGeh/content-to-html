{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module ProjDB.ToWebDoc(
	projectsPage,
	artistsPage,
	projectToArticle,
	artistToArticle,
	-- artistsList, -- needed?
) where

import ProjDB.Types as ProjDB
import WebDocumentStructure.Types as WebDocs

import qualified Data.Text as T
import Data.Maybe


projectsPage ::
	T.Text -> (Project -> Bool) -> ProjDB
	-> WebDocs.Page
projectsPage title filterProjects db =
	WebDocs.Page title $
	catMaybes $
	(map $ flip projectToArticle db) $
	filter filterProjects $
	catMaybes $
	(map $ flip ProjDB.lookupProject db) $
	ProjDB.allProjects db

artistsPage ::
	T.Text -> (ArtistKey -> Bool) -> ProjDB
	-> WebDocs.Page
artistsPage title filterFunc db =
	WebDocs.Page title $
	catMaybes $
	(map $ flip artistToArticle db) $
	filter filterFunc $
	ProjDB.allArtists db

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
