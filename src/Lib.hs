{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Lib(
	Config(..),
	PluginsConfig,
	pluginNames, PluginConfig(..),
	--ProjDBConfig, mlConfig,
	--DirConfig(..), defDirConfig,
	runHomepage,
	URI, toURI, fromURI,
	PluginName,
	loadAttributesConfig
) where

import qualified Plugins
import qualified Plugins.HierarchicWebsite as Site
import qualified Plugins.ProjDB as ProjDB
import qualified Plugins.Form as Form
import Types.Resource
-- import qualified Types.WebDocument as WebDoc
import Types.WebDocument.ToHtml
import Types.WebDocument.AttributesConfig
import Types.URI

import qualified Lucid

import Web.Spock
import Web.Spock.Config
import Control.Monad.State
import qualified Data.Map as M
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Maybe


pluginNames :: [PluginName]
pluginNames = M.keys plugins

plugins :: M.Map PluginName Plugins.LoaderCont
plugins =
	M.fromList $
	[ ("projDB", Plugins.LoaderCont $ ProjDB.load)
	, ("website", Plugins.LoaderCont $ Site.load)
	, ("form", Plugins.LoaderCont $ Form.load)
	]

type PluginName = String

data Config
	= Config {
		config_port :: Int,
		config_pluginsConfig :: PluginsConfig,
		config_attributesConfig :: AttributesCfg
	}
	deriving (Show, Read)

type PluginsConfig =
	M.Map PluginName PluginConfig

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
data Session
	= Session {
		session_lastViewReq :: Request
	}
type GlobalState = Plugins.Plugins


runHomepage :: Config -> IO ()
runHomepage Config{..} =
	handleErrors' $
	do
		pluginsInitState <- Plugins.loadPlugins $ pluginsLoadParams config_pluginsConfig
		let initState = pluginsInitState
		spockCfg <- lift $ calcSpockCfg $ initState
		liftIO $ runSpock config_port $
				spock spockCfg $
				spockRoutes config_attributesConfig
	where
		calcSpockCfg initState' =
			defaultSpockCfg sessionInit PCNoDatabase initState'
		sessionInit =
			Session $ (toURI "undefined", [])
		handleErrors' x =
			runExceptT x
			>>= either putStrLn return

spockRoutes :: AttributesCfg -> RoutesM ()
spockRoutes attributesConfig =
	getState >>= \pluginsState ->
	hookAny GET $ ( . uriFromList . map T.unpack) $ \fullUri ->
	let
		(uriPref, req) = uriSplitPrefix $ fullUri
	in
		-- ((liftIO $ putStrLn $ concat [ "req: ", show fullUri, " parsed as ", show (uriPref, req) ]) >>) $
		handleErrors $
		lift params >>= \reqParams ->
			do
				(mNewPage, _) <-
					runStateT `flip` pluginsState $
					Plugins.requestToPlugins uriPref (req, reqParams)
				case mNewPage of
					Just newPage ->
						sendResource attributesConfig (fullUri, reqParams) newPage
					Nothing ->
						lift $
						readSession >>= \session ->
							redirect $ T.pack $ fromURI $ fst $ session_lastViewReq session

sendResource ::
	AttributesCfg
	-> Request
	-> Resource
	-> ExceptT String (ActionCtxT ctx (WebStateM conn Session st)) b
sendResource attributesConfig req resource =
	case resource of
		FullPageResource page ->
			do
				lift $ writeSession $ Session req
				lift . html . LT.toStrict . Lucid.renderText . pageWithNavToHtml attributesConfig $ page
		PageResource page ->
			(lift . html . LT.toStrict . Lucid.renderText . sectionToHtml (attributes_sectionHeading attributesConfig) (attributes_section attributesConfig)) page
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
