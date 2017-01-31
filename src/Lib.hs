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
	--DirConfig(..), defDirConfig,
	runHomepage,
	URI, toURI, fromURI,
	Plugins.PluginName
) where

import qualified Plugins
import qualified Plugins.HierarchicWebsite as Site
import qualified Plugins.ProjDB as ProjDB
import WebDocumentStructure --( URI, toURI, fromURI )
import Types

import qualified Lucid

import Web.Spock.Safe
import Control.Monad.State
import qualified Data.Map as M
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Maybe


pluginNames :: [String]
pluginNames = M.keys plugins

plugins :: M.Map Plugins.PluginName Plugins.LoaderCont
plugins =
	M.fromList $
	[ ("projDB", Plugins.LoaderCont $ ProjDB.load)
	, ("website", Plugins.LoaderCont $ Site.load)
	]

data Config
	= Config {
		config_port :: Int,
		config_pluginsConfig :: PluginsConfig
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

type RoutesM = SpockM DBConn Session GlobalState

type DBConn = ()
type Session = ()
type GlobalState = Plugins.Plugins

runHomepage :: Config -> IO ()
runHomepage Config{..} =
	handleErrors' $
	do
		pluginsState <- Plugins.loadPlugins $ pluginsLoadParams config_pluginsConfig
		liftIO $ runSpock config_port $
				spock (spockCfg $ pluginsState) $
				spockRoutes
	where
		spockCfg initState' =
			defaultSpockCfg () PCNoDatabase initState'
		handleErrors' x =
			runExceptT x
			>>= either putStrLn return

spockRoutes :: RoutesM ()
spockRoutes =
	getState >>= \pluginsState ->
	hookAny GET $ \uriParts ->
		handleErrors $
			-- first, try to redirect the request to plugins:
			( lift params >>= \reqParams ->
				case uriParts of
					(uriPref:req) ->
						do
							(page, _) <- runStateT `flip` pluginsState $ Plugins.requestToPlugins (toURI $ T.unpack uriPref) (calcRouteKey req, M.fromList reqParams)
							sendResource page
					_ -> return ()
			)
	where
		calcRouteKey r = toURI (T.unpack $ T.intercalate "/" r)
		sendResource resource =
			case resource of
				FullPageResource page ->
					(lift . html . LT.toStrict . Lucid.renderText . pageWithNavToHtml) page
				PageResource page ->
					(lift . html . LT.toStrict . Lucid.renderText . sectionToHtml) page
				FileResource FileResInfo{..} ->
					lift $ file (fromResType $ fileRes_type) $ fileRes_file

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
