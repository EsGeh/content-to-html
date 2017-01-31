{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Monad.Except


projectsPage ::
	(Monad m, MonadError String m) =>
	(Project -> Bool)
	-> ReadDBT m WebDocs.Section
projectsPage filterProjects =
	WebDocs.mainSection <$>
	do
		projects <- select filterProjects
		mapM projectToSection projects

artistsPage ::
	(Monad m, MonadError String m) =>
	(Artist -> Bool)
	-> ReadDBT m WebDocs.Section
artistsPage filterFunc =
	WebDocs.mainSection <$>
	do
		artists <- select filterFunc
		mapM artistToSection artists

projectToSection ::
	(Monad m, MonadError String m) =>
	Project -> ReadDBT m Section
projectToSection Project{..} =
	do
		return $
			mainSectionWithTitle (fromProjectKey project_name) $
			map (section . projDataToWebContent) project_data

projDataToWebContent :: ProjectData -> WebContent
projDataToWebContent x =
	case x of
		ProjDB.Audio uri -> WebDocs.Audio uri
		ProjDB.Document DocumentInfo{..} ->
			Download $ DownloadInfo ("download " `T.append` doc_descr) doc_uri

artistToSection ::
	(Monad m, MonadError String m) =>
	Artist -> ReadDBT m Section
artistToSection artist =
	do
		projects <-
			select $ \proj -> artist_name artist `elem` project_artist proj
		return $
			mainSectionWithTitle (fromArtistKey $ artist_name artist) $ [
				mainSectionWithTitle "projects" $ map section [
					Text $
					T.intercalate ", " $
					map (fromProjectKey . project_name) $
					projects
				]
			]
