{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Plugins.ProjDB.ToWebDoc(
	projectsPage,
	artistsPage,
	projectToSection,
	artistToSection,
	-- artistsList, -- needed?
) where

import Plugins.ProjDB.Types as ProjDB
import WebDocumentStructure.Types as WebDocs

import qualified Data.Text as T
import Data.Maybe


projectsPage ::
	(Project -> Bool) -> ProjDB
	-> WebDocs.Section
projectsPage filterProjects db =
	WebDocs.mainSection $
	catMaybes $
	(map $ flip projectToSection db) $
	filter filterProjects $
	catMaybes $
	(map $ flip ProjDB.lookupProject db) $
	ProjDB.allProjects db

artistsPage ::
	(ArtistKey -> Bool) -> ProjDB
	-> WebDocs.Section
artistsPage filterFunc db =
	WebDocs.mainSection $
	catMaybes $
	(map $ flip artistToSection db) $
	filter filterFunc $
	ProjDB.allArtists db

projectToSection :: Project -> ProjDB -> Maybe Section
projectToSection Project{..} _ =
	do
		{-
		artist <-
			catMaybes $ -- ??
			map (flip lookupArtist db) $
			project_artist project
		-}
		return $
			mainSectionWithTitle project_name $
			map (section . projDataToWebContent) project_data

projDataToWebContent :: ProjectData -> WebContent
projDataToWebContent x =
	case x of
		ProjDB.Audio uri -> WebDocs.Audio uri
		ProjDB.Document DocumentInfo{..} ->
			Download $ DownloadInfo ("download " `T.append` doc_descr) doc_uri

artistToSection :: ArtistKey -> ProjDB -> Maybe Section
artistToSection key db =
	do
		artist <- lookupArtist key db
		let projects =
			projectsFromArtist (artist_name artist) db
			:: [Project]
		return $
			mainSectionWithTitle (artist_name artist) $ [
				mainSectionWithTitle "projects" $ map section [
					Text $
					T.intercalate ", " $
					map project_name $
					projects
				]
			]
