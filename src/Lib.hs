{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Lib(
	Config(..),
	ProjDBConfig, mlConfig,
	ContentConfig(..),
	runHomepage
) where

import qualified CMS
import qualified WebDocumentStructure as WebDocs
import qualified ProjDB
import qualified ProjDB.ToWebDoc -- as .Html
import RoutesMonad
import qualified Html
import Utils( mapSnd )

import Web.Spock.Safe
import Lucid
import System.FilePath.Posix
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Except
import qualified Data.Text as T
import Data.Maybe

type ProjDBConfig = ProjDB.Config

data Config
	= Config {
		config_port :: Int,
		config_content :: ContentConfig,
		config_projDB :: ProjDB.Config
	}
	deriving (Show, Read)

data ContentConfig
	= ContentConfig {
		config_pagesDir :: FilePath,
		config_cssDir :: FilePath,
		config_dataDir :: FilePath
	}
	deriving( Show, Read )

runHomepage :: Config -> IO ()
runHomepage conf =
	let
		port = config_port conf
		contentCfg = config_content conf
	in
		handleErrors' $
		do
			db <- ProjDB.loadState (config_projDB conf) -- <|> ProjDB.newState
			content <-
				(loadContent db contentCfg `catchError` \e -> throwError ("error while loading content: " ++ e))
			liftIO $ runSpock port $
					spock (spockCfg $ GlobalState content db) $
					runRoutes (routes conf)
	where
		spockCfg initState =
			defaultSpockCfg () PCNoDatabase initState
		handleErrors' x =
			runExceptT x
			>>= either putStrLn return

loadContent ::
	(MonadIO m, MonadError String m) =>
	ProjDB.ProjDB -> ContentConfig -> m CMS.Routes
loadContent db ContentConfig{..} =
	((\x -> do{ liftIO $ print x; return x} ) =<<) $

	(CMS.addRoute "/content/ownProjects" $ CMS.PageResource $
		WebDocs.Page "own projects" $
		catMaybes $
		(map $ flip ProjDB.ToWebDoc.projectToArticle db) $
		filter (("Samuel Gfrörer" `elem`) . ProjDB.project_artist) $
		catMaybes $
		(map $ flip ProjDB.lookupProject db) $
		ProjDB.allProjects db
	) <$>
	(CMS.addRoute "/content/artists" $ CMS.PageResource $
		WebDocs.Page "artists I like" $
		catMaybes $
		(map $ flip ProjDB.ToWebDoc.artistToArticle db) $
		filter ((/="Samuel Gfrörer")) $
		ProjDB.allArtists db
	) <$>

	(
		CMS.combineRoutes <$>
		sequence
		[
			CMS.loadFilesInDir `flip` config_pagesDir $ \path ->
				case takeExtension path of
					".yaml" ->
						Just <$>
							( CMS.URI $ "/" </> "content" </> dropExtension path, ) <$>
							CMS.PageResource <$>
							CMS.loadYaml (config_pagesDir </> path)
					_ -> return $ Nothing
		, CMS.loadFilesInDir `flip` config_dataDir $
				return . CMS.defLoadDirInfo "data" config_dataDir
		, CMS.loadFilesInDir `flip` config_cssDir $
				return . CMS.defLoadDirInfo "css" config_cssDir
		]
	)

mlConfig :: FilePath -> ProjDBConfig
mlConfig = ProjDB.Config

runRoutes ::
	RoutesM GlobalState () -> SpockM () () GlobalState ()
runRoutes =
	prehook $
	initRoutes <$> getState

routes :: Config -> RoutesM GlobalState ()
routes _ = 
	do
		subRoutes "/content" globState_cms $
			contentRoutes
		subRoutes "/css" globState_cms $
			resourceRoutes
		subRoutes "/data" globState_cms $
			resourceRoutes

contentRoutes :: RoutesM CMS.Routes ()
contentRoutes =
	do
		methodGetVar "/" $ \(_ :: FilePath) ->
			handleErrors $
			do
				requested <- lift $ getRoute
				routes' <- lift $ getCtx
				resource <-
					CMS.findPage requested routes'
				case resource of
					CMS.PageResource page ->
						(lift . html . Html.renderPage . fullPage (CMS.routes_pages routes') requested) page
					_ -> throwError $ "resource not found!"

resourceRoutes :: RoutesM CMS.Routes ()
resourceRoutes =
	do
		methodGetVar "/" $ \(_ :: FilePath) ->
			handleErrors $
			do
				requested <- lift $ getRoute
				content <- lift $ getCtx
				resource <-
					CMS.findPage requested content
				case resource of
					CMS.FileResource info ->
						lift $ file (CMS.fromResType $ CMS.fileRes_type info) $ CMS.fileRes_file info
					_ -> throwError $ "resource not found!"

fullPage :: CMS.PageRoutes -> FilePath -> WebDocs.Page -> Html ()
fullPage content route page =
	Html.basePage (WebDocs.page_title page) $
	(Html.nav $
		Html.calcNavLinks route $
		map (mapSnd WebDocs.page_title) $
		M.toList $
		content
	)
	<>
	WebDocs.pageToHtml page

handleErrors ::
	ExceptT String (ActionM ctx) a
	-> ActionM ctx a
handleErrors x =
	runExceptT x
	>>=
	either
		(\e -> text $ T.pack $ "404: resource not found. error: " ++ e)
		return
