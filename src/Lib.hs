{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Lib(
	Config(..),
	Plugins.PluginsConfig(..),
	Plugins.MainPluginsConfig,
	mainPluginNames, Plugins.MainPluginConfig(..),
	Plugins.EmbeddablesConfig, embeddableNames,
	runHomepage,
	URI, toURI, fromURI,
	Plugins.MainPluginName,
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

import qualified Web.Spock as Spock
import qualified Web.Spock.Config as Spock
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT


data Config
	= Config {
		config_port :: Int,
		config_pluginsConfig :: Plugins.PluginsConfig,
		config_attributesConfig :: AttributesCfg
	}
	deriving (Show, Read)

mainPluginNames :: [Plugins.MainPluginName]
mainPluginNames = M.keys mainPluginsByName

embeddableNames :: [Plugins.EmbeddableName]
embeddableNames =
	M.keys (
		embeddableLoadersByName
		:: M.Map Plugins.EmbeddableName (Plugins.EmbeddableLoader (ExceptT String IO))
	)

mainPluginsByName ::
	M.Map Plugins.MainPluginName Plugins.MainLoaderContainer
mainPluginsByName =
	M.fromList $
	[ ("website", Plugins.MainLoaderContainer Site.load)
	]

embeddableLoadersByName ::
	(MonadIO m, MonadError String m) =>
	M.Map Plugins.EmbeddableName (Plugins.EmbeddableLoader m)
embeddableLoadersByName =
	M.fromList $
	[ ("projDB", ProjDB.load)
	, ("form", Form.load)
	]

type RoutesM = Spock.SpockM DBConn Session GlobalState

type DBConn = ()
data Session
	= Session {
		session_lastViewReq :: Request
	}

type GlobalState = Plugins.AllPlugins


runHomepage :: Config -> IO ()
runHomepage Config{..} =
	handleErrors' $
	do
		pluginsInitState <-
			Plugins.loadAllPlugins
				mainPluginsByName
				embeddableLoadersByName
				config_pluginsConfig
		let initState = pluginsInitState
		spockCfg <- lift $ calcSpockCfg $ initState
		liftIO $ Spock.runSpock config_port $
				Spock.spock spockCfg $
				spockRoutes config_attributesConfig
	where
		calcSpockCfg initState' =
			Spock.defaultSpockCfg sessionInit Spock.PCNoDatabase initState'
		sessionInit =
			Session $ (toURI "undefined", [])
		handleErrors' x =
			runExceptT x
			>>= either putStrLn return

spockRoutes :: AttributesCfg -> RoutesM ()
spockRoutes attributesConfig =
	Spock.getState >>= \pluginsState ->
	Spock.hookAny Spock.GET $ ( . uriFromList . map T.unpack) $ \fullUri ->
	let
		(uriPref, req) = uriSplitPrefix $ fullUri
	in
		-- ((liftIO $ putStrLn $ concat [ "req: ", show fullUri, " parsed as ", show (uriPref, req) ]) >>) $
		handleErrors $
		lift Spock.params >>= \reqParams ->
			do
				(mNewPage, _) <-
					runStateT `flip` pluginsState $
					Plugins.requestToPlugins uriPref (req, reqParams)
					<|>
					Plugins.requestToEmbeddables (fromURI uriPref) (req, reqParams)
				case mNewPage of
					Just newPage ->
						sendResource attributesConfig (fullUri, reqParams) newPage
					Nothing ->
						lift $
						Spock.readSession >>= \session ->
							Spock.redirect $ T.pack $ fromURI $ fst $ session_lastViewReq session

sendResource ::
	AttributesCfg
	-> Request
	-> Resource
	-> ExceptT String (Spock.ActionCtxT ctx (Spock.WebStateM conn Session st)) b
sendResource attributesConfig req resource =
	case resource of
		FullPageResource page ->
			do
				lift $ Spock.writeSession $ Session req
				lift . Spock.html . LT.toStrict . Lucid.renderText . pageWithNavToHtml attributesConfig $ page
		PageResource page ->
			(lift . Spock.html . LT.toStrict . Lucid.renderText . sectionToHtml (attributes_sectionHeading attributesConfig) (attributes_section attributesConfig)) page
		FileResource FileResInfo{..} ->
			lift $ Spock.file (fromResType $ fileRes_type) $ fileRes_file

handleErrors ::
	MonadIO m =>
	ExceptT String (Spock.ActionCtxT ctx m) a
	-> Spock.ActionCtxT ctx m a
handleErrors x =
	runExceptT x
	>>=
	either
		(\e -> Spock.text $ T.pack $ "404: resource not found. error: " ++ e)
		return
