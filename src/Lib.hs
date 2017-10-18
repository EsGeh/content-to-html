{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Lib(
	Config(..),
	MainPluginsConfig,
	mainPluginNames, MainPluginConfig(..),
	EmbeddablesConfig, embeddableNames,
	runHomepage,
	URI, toURI, fromURI,
	MainPluginName,
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
import qualified Utils.Yaml as Yaml

import qualified Lucid

import Web.Spock
import Web.Spock.Config
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Maybe


type MainPluginName = String

data Config
	= Config {
		config_port :: Int,
		config_mainPluginsConfig :: MainPluginsConfig,
		config_embeddablesConfig :: EmbeddablesConfig,
		config_attributesConfig :: AttributesCfg
	}
	deriving (Show, Read)

type MainPluginsConfig =
	M.Map MainPluginName MainPluginConfig

type EmbeddablesConfig =
	M.Map Plugins.EmbeddableName FilePath

data MainPluginConfig = 
	MainPluginConfig {
		mainPlugin_uri :: URI,
		mainPlugin_configFile :: FilePath
	}
	deriving (Show, Read)

mainPluginNames = M.keys mainPlugins
embeddableNames =
	M.keys (
		embeddableLoaders
		:: M.Map Plugins.EmbeddableName (FilePath -> Plugins.EmbeddableLoader (ExceptT String IO))
	)

mainPlugins ::
	M.Map MainPluginName Plugins.MainLoaderContainer
mainPlugins =
	M.fromList $
	[ ("website", Plugins.MainLoaderContainer Site.load)
	]

embeddableLoaders ::
	(MonadIO m, MonadError String m) =>
	M.Map Plugins.EmbeddableName (FilePath -> Plugins.EmbeddableLoader m)
embeddableLoaders =
	M.fromList $
	[ ("projDB", ProjDB.load)
	, ("form", Form.load)
	]

loadAllPlugins ::
	forall m .
	(MonadIO m, MonadError String m) =>
	Config -> m Plugins.AllPlugins
loadAllPlugins config@Config{..} =
	do
		--liftIO $ putStrLn $ show config
		liftIO $ putStrLn $ "configuring embeddables..."
		embeddableLoaders <-
			preloadEmbeddables config_embeddablesConfig :: m (M.Map Plugins.EmbeddableName (Plugins.EmbeddableLoader m))
		liftIO $ putStrLn $ "loading main plugins..."
		(mainPlugins, embeddables)  <-
			fmap bundlePlugins $
			loadMainPlugins embeddableLoaders config_mainPluginsConfig
		liftIO $ putStrLn $ "loading done"
		return Plugins.AllPlugins{
			Plugins.allPlugins_mainPlugins = mainPlugins,
			Plugins.allPlugins_embeddables = embeddables
		}
	where
		bundlePlugins ::
			[(URI, Plugins.MainPluginStateCont, Plugins.Embeddables)]
			-> (Plugins.MainPlugins, Plugins.Embeddables)
		bundlePlugins list =
			( M.fromList $ map `flip` list $ \(uri, plugin, _) -> (uri, plugin)
			, M.unions $ map `flip` list $ \(_, _, embeddables) -> embeddables
			)

preloadEmbeddables :: 
	forall m .
	(MonadIO m, MonadError String m) =>
	EmbeddablesConfig -> m (M.Map Plugins.EmbeddableName (Plugins.EmbeddableLoader m))
preloadEmbeddables embeddablesConfig=
	fmap M.fromList $
	forM (M.toList embeddablesConfig) $ \(name, configFile) ->
	do
		loader' <-
			maybe (throwError $ "embeddable \"" ++ name ++ "\" not found") return $
				M.lookup name embeddableLoaders
		return (name, loader' configFile)

loadMainPlugins ::
	forall m .
	(MonadIO m, MonadError String m) =>
	M.Map Plugins.EmbeddableName (Plugins.EmbeddableLoader m)
	-> MainPluginsConfig -> m [(URI, Plugins.MainPluginStateCont, Plugins.Embeddables)]
loadMainPlugins embeddableLoaders config =
	-- fmap (\x -> (M.fromList $ map fst x, _ $ map snd x )) $
	forM (M.toList config) $ \(name, MainPluginConfig{..}) ->
	do
		liftIO $ putStrLn $ concat ["loading plugin \"", name, "\"..." ]
		Plugins.MainLoaderContainer mainLoader <-
			maybe (throwError $ "main plugin \"" ++ name ++ "\" not found") return $
			M.lookup name mainPlugins :: m Plugins.MainLoaderContainer
		(mainPlugin, initState, embeddableParams) <- mainLoader mainPlugin_configFile
		--liftIO $ putStrLn $ concat ["embeddableParams: ", show embeddableParams]
		embeddables <-
			fmap M.fromList $
			forM (M.toList embeddableParams) $ \(instanceId, (embeddableName, params)) ->
				do
					embeddableLoader <-
						maybe (throwError $ "embeddable \"" ++ embeddableName ++ "\" not found") return $
						M.lookup embeddableName embeddableLoaders
						:: m (Plugins.EmbeddableLoader m)
					embeddable <- embeddableLoader params :: m Plugins.EmbeddableStateCont
					return (instanceId, embeddable)
			-- :: m Plugins.Embeddables
		--liftIO $ putStrLn $ "bla2"
		return $
			( mainPlugin_uri
			, Plugins.MainPluginStateCont (mainPlugin, initState)
			, embeddables
			)

type RoutesM = SpockM DBConn Session GlobalState

type DBConn = ()
data Session
	= Session {
		session_lastViewReq :: Request
	}

type GlobalState = Plugins.AllPlugins


runHomepage :: Config -> IO ()
runHomepage config@Config{..} =
	handleErrors' $
	do
		pluginsInitState <- loadAllPlugins $ config
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
					<|>
					Plugins.requestToEmbeddables (fromURI uriPref) (req, reqParams)
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
