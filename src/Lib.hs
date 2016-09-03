{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib(
	Config(..),
	ProjDBConfig, mlConfig,
	ContentConfig(..),
	runHomepage
) where

import qualified CMS
import qualified ProjDB
import qualified ProjDB.Html -- as .Html
import RoutesMonad
import RenderPage
import qualified Html

import Web.Spock.Safe
import Lucid
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Except
import qualified Data.Text as T

type ProjDBConfig = ProjDB.Config

data ContentConfig
	= ContentConfig {
		config_pagesDir :: FilePath,
		config_cssDir :: FilePath,
		config_dataDir :: FilePath
	}
	deriving( Show, Read )

loadContent ::
	(MonadIO m, MonadError String m) =>
	ProjDB.ProjDB -> ContentConfig -> m CMS.Routes
loadContent db cfg =
	do
		(M.insert "/content/artists" $ CMS.PageResource $ ProjDB.Html.artistsList "artists I like" db) <$>
			CMS.load
				[config_pagesDir cfg]
				[ CMS.FileResInfo "style/css" "css"
				, CMS.FileResInfo "audio/mpeg" "data"
				]

mlConfig :: FilePath -> ProjDBConfig
mlConfig = ProjDB.Config

{-
contentConfig :: FilePath -> FilePath -> FilePath -> ContentConfig
contentConfig = CMS.Config
-}

data Config
	= Config {
		config_port :: Int,
		config_content :: ContentConfig,
		config_projDB :: ProjDB.Config
	}
	deriving (Show, Read)

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
		{-
		subRoutes "/projects" globState_projDB $
			projectsRoutes
		-}

{-
projectsRoutes :: RoutesM ProjDB.ProjDB ()
projectsRoutes =
	methodGet "/" $
		do
			(db :: ProjDB.ProjDB) <- getCtx -- :: ActionM ProjDB.ProjDB ProjDB.ProjDB
			liftIO $ print $ ProjDB.allArtists db
			html $ renderPage $
				mconcat $ map CMS.articleToHtml $
				map (fromMaybe undefined . flip ProjDB.Html.artistToArticle db) $
				ProjDB.allArtists db
-}

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
						(lift . html . renderPage . fullPage (CMS.routes_pages routes') requested) page
					_ -> throwError $ "resource not found!"

handleErrors ::
	ExceptT String (ActionM ctx) a
	-> ActionM ctx a
handleErrors x =
	runExceptT x
	>>=
	either
		(\e -> text $ T.pack $ "404: resource not found. error: " ++ e)
		return

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
						lift $ file (CMS.fileRes_type info) $ CMS.fileRes_file info
					_ -> throwError $ "resource not found!"

fullPage :: CMS.PageRoutes -> FilePath -> CMS.Page -> Html ()
fullPage content route page =
	Html.basePage (CMS.page_title page) $
	(Html.nav $
		Html.calcNavLinks route $
		map (mapSnd CMS.page_title) $
		M.toList $
		content
	)
	<>
	CMS.pageToHtml page

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a,b) = (a, f b)
