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
			WebDocs.MainSection $ (defSectionInfo projectPages){
				section_style = defStyleInfo{ style_class = Just "projects" }
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
			WebDocs.MainSection $ (defSectionInfo artistPages){
				section_style = defStyleInfo{ style_class = Just "artists" }
			}

projectToSection ::
	(Monad m, MonadError String m) =>
	Project -> ReadDBT m Section
projectToSection Project{..} =
	do
		return $
			WebDocs.MainSection $
				(defSectionInfo $ map (section . projDataToWebContent) project_data){
					section_title = Just $ fromProjectKey project_name,
					section_style = defStyleInfo{ style_class = Just "project" }
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
						section_style = defStyleInfo{ style_class = Just "artist" }
					}
			_ ->
				return $
					MainSection $
					(defSectionInfo $ [
						mainSectionWithTitle "projects" $ map section [
							Text $
							T.intercalate ", " $
							map (fromProjectKey . project_name) $
							projects
						]
					]){
						section_title = Just $ fromArtistKey $ artist_name artist,
						section_style = defStyleInfo{ style_class = Just "artist" }
					}
