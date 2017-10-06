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
import Plugins.ProjDB.DB
import Types.WebDocument as WebDocs

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Except


projectsPage ::
	(Monad m, MonadError String m) =>
	(Project -> Bool)
	-> ReadDBT m WebDocs.Section
projectsPage filterProjects =
	do
		projects <- select filterProjects
		projectPages <- mapM projectToSection projects
		return $
			WebDocs.SectionNode $ (defSectionInfo projectPages){
				section_attributes = M.fromList [("class", "projects")]
			}

artistsPage ::
	(Monad m, MonadError String m) =>
	(Artist -> Bool)
	-> ReadDBT m WebDocs.Section
artistsPage filterFunc =
	do
		artists <- select filterFunc
		artistPages <- mapM artistToSection artists
		return $
			WebDocs.SectionNode $ (defSectionInfo artistPages){
				section_attributes = M.fromList [("class", "artists")]
			}

projectToSection ::
	(Monad m, MonadError String m) =>
	Project -> ReadDBT m Section
projectToSection Project{..} =
	do
		return $
			WebDocs.SectionNode $
				(defSectionInfo $ map (section . projDataToWebContent) project_data){
					section_title = Just $ fromProjectKey project_name,
					section_attributes = M.fromList [("class", "project")]
				}

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
		case projects of
			[] ->
				return $
					SectionEntry $
					(defSectionInfo $ Text $ "no more info") {
						section_title = Just $ fromArtistKey $ artist_name artist,
						section_attributes = M.fromList [("class", "artist")]
					}
			_ ->
				return $
					SectionNode $
					(defSectionInfo $ [
						mainSectionWithTitle "projects" $ map section [
							Text $
							T.intercalate ", " $
							map (fromProjectKey . project_name) $
							projects
						]
					]){
						section_title = Just $ fromArtistKey $ artist_name artist,
						section_attributes = M.fromList [("class", "artist")]
					}
