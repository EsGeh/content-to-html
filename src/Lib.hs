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
	URI, toURI, fromURI,
	Plugins.PluginName
) where

import qualified Plugins
import qualified Routes
import WebDocumentStructure
import qualified ProjDB
import qualified Html

import Web.Spock.Safe
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
		plugin_uri :: URI,
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
		contentTree <- loadContent config_content
		liftIO $ runSpock config_port $
				spock (spockCfg $ pluginsState) $
				spockRoutes sharedData contentTree (toURI <$> config_userCSS)
	where
		spockCfg initState' =
			defaultSpockCfg () PCNoDatabase initState'
		handleErrors' x =
			runExceptT x
			>>= either putStrLn return

spockRoutes :: Routes.Routes -> Content -> Maybe URI -> RoutesM ()
spockRoutes routes content mUserCss =
	getState >>= \pluginsState ->
	hookAny GET $ \uriParts ->
		handleErrors $
			-- first, try to redirect the request to plugins:
			( lift params >>= \reqParams ->
				case uriParts of
					(uriPref:req) ->
						do
							(page, _) <- Plugins.routeToPlugins pluginsState (toURI $ T.unpack uriPref) (calcRouteKey req, M.fromList reqParams)
							sendResource page
					_ -> return ()
			)
			<|>
			-- otherwise:
			(do
				let resKey = calcRouteKey uriParts
				resource <- Routes.findPage resKey routes
				sendResource resource
			)
	where
		calcRouteKey r = toURI (T.unpack $ T.intercalate "/" r)
		sendResource resource =
			case resource of
				PageResource page ->
					(lift . html . Html.renderPage . fullPage mUserCss content) page
				FileResource FileResInfo{..} ->
					lift $ file (fromResType $ fileRes_type) $ fileRes_file

loadSharedData ::
	(MonadIO m, MonadError String m) =>
	[DirConfig] -> m Routes.Routes
loadSharedData sharedDirs =
	--((\x -> do{ liftIO $ print x; return x} ) =<<) $
	fmap Routes.combineRoutes $
	forM sharedDirs $ \DirConfig{..} ->
		Routes.loadFilesInDir `flip` dirConfig_path $
		Routes.defLoadDirInfo dirConfig_uriPrefix dirConfig_path

fullPage :: Maybe URI -> Content -> Page -> Html ()
fullPage mUserCss content page =
	Html.basePage mUserCss (page_title page) $
	Html.nav content
	<>
	pageToHtml page

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
