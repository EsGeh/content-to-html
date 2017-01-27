{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Lib(
	Config(..),
	ProjDBConfig, mlConfig,
	ContentConfig(..),
	DirConfig(..), defDirConfig,
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
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Except
import qualified Data.Text as T

type ProjDBConfig = ProjDB.Config

data Config
	= Config {
		config_port :: Int,
		config_content :: ContentConfig,
		config_projDB :: ProjDB.Config,
		config_userCSS :: Maybe FilePath
	}
	deriving (Show, Read)

data ContentConfig
	= ContentConfig {
		contentDirs :: [DirConfig]
	}
	deriving( Show, Read )

data DirConfig
	= DirConfig {
		dirConfig_path :: FilePath,
		dirConfig_uriPrefix :: FilePath
	}
	deriving( Show, Read )

defDirConfig :: FilePath -> DirConfig
defDirConfig path = DirConfig path path

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
					spock (spockCfg $ initState) $
					spockRoutes content (CMS.toURI <$> config_userCSS conf)
	where
		spockCfg initState' =
			defaultSpockCfg () PCNoDatabase initState'
		handleErrors' x =
			runExceptT x
			>>= either putStrLn return

spockRoutes :: CMS.Routes -> Maybe CMS.URI -> RoutesM ()
spockRoutes routes mUserCss =
	hookAny GET $ (. calcRouteKey) $ \path ->
		handleErrors $
		do
			resource <- CMS.findPage path routes
			case resource of
				CMS.PageResource page ->
					(lift . html . Html.renderPage . fullPage mUserCss (CMS.routes_pages routes) path) page
				CMS.FileResource CMS.FileResInfo{..} ->
					lift $ file (CMS.fromResType $ fileRes_type) $ fileRes_file
	where
		calcRouteKey r = CMS.toURI (T.unpack $ T.intercalate "/" r)

loadContent ::
	(MonadIO m, MonadError String m) =>
	ProjDB.ProjDB -> ContentConfig -> m CMS.Routes
loadContent db ContentConfig{..} =
	((\x -> do{ liftIO $ print x; return x} ) =<<) $
	fmap (
		(CMS.addRoute (CMS.toURI "/content/artists") $ CMS.PageResource $
			ProjDB.ToWebDoc.artistsPage "artists I like" `flip` db $ (/="Samuel Gfrörer")
		)
		.
		(CMS.addRoute (CMS.toURI "/content/ownProjects") $ CMS.PageResource $
			ProjDB.ToWebDoc.projectsPage "own projects" `flip` db $
				(\p -> "Samuel Gfrörer" `elem` ProjDB.project_artist p)
		)
	) $
	fmap CMS.combineRoutes $
	forM contentDirs $ \DirConfig{..} ->
		CMS.loadFilesInDir `flip` dirConfig_path $
		CMS.defLoadDirInfo dirConfig_uriPrefix dirConfig_path

mlConfig :: FilePath -> ProjDBConfig
mlConfig = ProjDB.Config

fullPage :: Maybe CMS.URI -> CMS.PageRoutes -> CMS.URI -> WebDocs.Page -> Html ()
fullPage mUserCss content route page =
	Html.basePage mUserCss (WebDocs.page_title page) $
	(Html.nav $
		Html.calcNavLinks route $
		map (mapSnd WebDocs.page_title) $
		M.toList $
		content
	)
	<>
	WebDocs.pageToHtml page

handleErrors ::
	MonadIO m =>
	ExceptT String (ActionCtxT ctx m) a
	-> ActionCtxT ctx m a
handleErrors x =
	runExceptT x
	>>=
	either
		(\e -> text $ T.pack $ "404: resource not found. error: " ++ e)
		return
