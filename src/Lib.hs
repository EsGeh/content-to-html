{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Lib(
	Config(..),
	PluginsConfig,
	pluginNames, PluginConfig(..),
	--ProjDBConfig, mlConfig,
	DirConfig(..), defDirConfig,
	runHomepage,
	CMS.toURI,
	Plugins.PluginName
) where

import qualified Plugins
import qualified ContentAndRoutes as CMS
import qualified WebDocumentStructure as WebDocs
import qualified ProjDB
--import qualified ProjDB.ToWebDoc -- as .Html
import qualified Html

import Web.Spock.Safe
import Lucid
import Data.Monoid
import qualified Data.Map as M
import Control.Monad.Except
import qualified Data.Text as T
import Data.Maybe
import Control.Applicative


pluginNames :: [String]
pluginNames = M.keys plugins

plugins :: M.Map Plugins.PluginName Plugins.LoaderCont
plugins =
	M.fromList $
	[("projDB", Plugins.LoaderCont $ ProjDB.load)
	]

data Config
	= Config {
		config_port :: Int,
		config_sharedDirs :: [DirConfig],
		config_pluginsConfig :: PluginsConfig,
		config_userCSS :: Maybe FilePath,
		config_content :: FilePath
	}
	deriving (Show, Read)

type PluginsConfig =
	M.Map Plugins.PluginName PluginConfig

data PluginConfig = 
	PluginConfig {
		plugin_uri :: CMS.URI,
		plugin_configFile :: FilePath
	}
	deriving (Show, Read)

pluginsLoadParams :: PluginsConfig -> Plugins.PluginsLoadParams
pluginsLoadParams pluginsCfg =
	M.fromList . catMaybes $
	map `flip` (M.toList plugins) $ \(name, loader) ->
		M.lookup name pluginsCfg >>= \PluginConfig{..} ->
			return $ (plugin_uri, (plugin_configFile, loader))

data DirConfig
	= DirConfig {
		dirConfig_path :: FilePath,
		dirConfig_uriPrefix :: FilePath
	}
	deriving( Show, Read )

type RoutesM = SpockM DBConn Session GlobalState

type DBConn = ()
type Session = ()
type GlobalState = Plugins.Plugins

defDirConfig :: FilePath -> DirConfig
defDirConfig path = DirConfig path path

runHomepage :: Config -> IO ()
runHomepage Config{..} =
	handleErrors' $
	do
		pluginsState <- Plugins.loadPlugins $ pluginsLoadParams config_pluginsConfig
		sharedData <-
			(loadSharedData config_sharedDirs `catchError` \e -> throwError ("error while loading sharedData: " ++ e))
		contentTree <- CMS.loadContent config_content
		liftIO $ runSpock config_port $
				spock (spockCfg $ pluginsState) $
				spockRoutes sharedData contentTree (CMS.toURI <$> config_userCSS)
	where
		spockCfg initState' =
			defaultSpockCfg () PCNoDatabase initState'
		handleErrors' x =
			runExceptT x
			>>= either putStrLn return

spockRoutes :: CMS.Routes -> CMS.Content -> Maybe CMS.URI -> RoutesM ()
spockRoutes routes content mUserCss =
	getState >>= \pluginsState ->
	hookAny GET $ \uriParts ->
		handleErrors $
			-- first, try to redirect the request to plugins:
			( lift params >>= \reqParams ->
				case uriParts of
					(uriPref:req) ->
						do
							(page, _) <- Plugins.routeToPlugins pluginsState (CMS.toURI $ T.unpack uriPref) (calcRouteKey req, M.fromList reqParams)
							sendResource page
					_ -> return ()
			)
			<|>
			-- otherwise:
			(do
				let resKey = calcRouteKey uriParts
				resource <- CMS.findPage resKey routes
				sendResource resource
			)
	where
		calcRouteKey r = CMS.toURI (T.unpack $ T.intercalate "/" r)
		sendResource resource =
			case resource of
				CMS.PageResource page ->
					(lift . html . Html.renderPage . fullPage mUserCss content) page
				CMS.FileResource CMS.FileResInfo{..} ->
					lift $ file (CMS.fromResType $ fileRes_type) $ fileRes_file

loadSharedData ::
	(MonadIO m, MonadError String m) =>
	[DirConfig] -> m CMS.Routes
loadSharedData sharedDirs =
	--((\x -> do{ liftIO $ print x; return x} ) =<<) $
	fmap CMS.combineRoutes $
	forM sharedDirs $ \DirConfig{..} ->
		CMS.loadFilesInDir `flip` dirConfig_path $
		CMS.defLoadDirInfo dirConfig_uriPrefix dirConfig_path

fullPage :: Maybe CMS.URI -> CMS.Content -> WebDocs.Page -> Html ()
fullPage mUserCss content page =
	Html.basePage mUserCss (WebDocs.page_title page) $
	Html.nav content
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
