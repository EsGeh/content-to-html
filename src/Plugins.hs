{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Plugins where

import Types.Resource
import Types.WebDocument
import Types.URI
import Utils.Lens

import qualified Data.Yaml as Yaml

import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Control.Monad.State as St
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Lens.Micro.Platform as Lens


-- The Plugin system consists of MainPlugins and Embeddables
data AllPlugins
	= AllPlugins{
		allPlugins_mainPlugins :: MainPlugins,
		allPlugins_embeddables :: Embeddables
	}

type MainPlugins = M.Map URI MainPluginStateCont
type Embeddables = M.Map EmbeddableInstanceID EmbeddableStateCont

type RunReqT state m a =
	St.StateT state (St.StateT AllPlugins m) a

-- |a "MainPlugin" is something that can answer requests by returning a 'Resource'
data MainPlugin state
	= MainPlugin {

		-- |answering an external request, optionally changing the view:
		mainPlugin_answerReq ::
			forall m . (MonadIO m, MonadError String m) =>
			Request -> RunReqT state m (Maybe Resource),

		mainPlugin_descr :: T.Text

	}

-- |container for a plugin bundled with its current state
data MainPluginStateCont =
	forall state . MainPluginStateCont (MainPlugin state, state)

-- |create a plugin and a corresponding initial state
type MainLoader state =
	forall m .
	(MonadIO m, MonadError String m) =>
	FilePath -> m (MainPlugin state, state, M.Map EmbeddableInstanceID (EmbeddableName, Yaml.Value))

data MainLoaderContainer = forall state . MainLoaderContainer (MainLoader state)


data Embeddable params state
	= Embeddable {

		-- |answering an external request, optionally changing the view:
		embeddable_answerReq ::
			forall m . (MonadIO m, MonadError String m) =>
			EmbeddableInstanceID
			-> params
			-> Request -> RunReqT state m (Maybe Resource),
			
		-- |answering a request from an other plugin
		embeddable_answerInternalReq ::
			forall m . (MonadIO m, MonadError String m) =>
			EmbeddableInstanceID
			-> params
			-> RunReqT state m Section,

		embeddable_descr :: T.Text

	}

defaultEmbeddable :: Embeddable params state
defaultEmbeddable =
	Embeddable {
		embeddable_answerInternalReq = \_ _ -> throwError "internal requests are not allowed!",
		embeddable_answerReq = \_ _ _ -> throwError "requests not allowed!",
		embeddable_descr = "defaultEmbeddable"
	}

type EmbeddableLoader m =
	FilePath -> Yaml.Value -> m EmbeddableStateCont

-- |container for a plugin bundled with its current state
data EmbeddableStateCont =
	forall params state . EmbeddableStateCont (Embeddable params state, params, state)

type EmbeddableInstanceID = String


type MainPluginName = String
type EmbeddableName = String


$(Lens.makeLensesWith lensRules' ''AllPlugins)

data PluginsConfig
	= PluginsConfig{
		config_mainPluginsConfig :: MainPluginsConfig,
		config_embeddablesConfig :: EmbeddablesConfig
	}
	deriving (Show, Read)

type MainPluginsConfig =
	M.Map MainPluginName MainPluginConfig

type EmbeddablesConfig =
	M.Map EmbeddableName FilePath

data MainPluginConfig = 
	MainPluginConfig {
		mainPlugin_uri :: URI,
		mainPlugin_configFile :: FilePath
	}
	deriving (Show, Read)

loadAllPlugins ::
	forall m .
	(MonadIO m, MonadError String m) =>
	M.Map MainPluginName MainLoaderContainer
	-> M.Map EmbeddableName (EmbeddableLoader m)
	-> PluginsConfig
	-> m AllPlugins
loadAllPlugins mainPluginsByName embeddableLoadersByName PluginsConfig{..} =
	do
		--liftIO $ putStrLn $ show config
		liftIO $ putStrLn $ "configuring embeddables..."
		embeddableLoaders <-
			preloadEmbeddables embeddableLoadersByName config_embeddablesConfig :: m (M.Map EmbeddableName (Yaml.Value -> m EmbeddableStateCont))
		liftIO $ putStrLn $ "loading main plugins..."
		(mainPlugins, embeddables)  <-
			fmap bundlePlugins $
			loadMainPlugins mainPluginsByName embeddableLoaders config_mainPluginsConfig
		liftIO $ putStrLn $ "loading done"
		return AllPlugins{
			allPlugins_mainPlugins = mainPlugins,
			allPlugins_embeddables = embeddables
		}
	where
		bundlePlugins ::
			[(URI, MainPluginStateCont, Embeddables)]
			-> (MainPlugins, Embeddables)
		bundlePlugins list =
			( M.fromList $ map `flip` list $ \(uri, plugin, _) -> (uri, plugin)
			, M.unions $ map `flip` list $ \(_, _, embeddables) -> embeddables
			)

preloadEmbeddables :: 
	forall m .
	(MonadIO m, MonadError String m) =>
	M.Map EmbeddableName (EmbeddableLoader m)
	-> EmbeddablesConfig
	-> m (M.Map EmbeddableName (Yaml.Value -> m EmbeddableStateCont))
preloadEmbeddables embeddableLoadersByName embeddablesConfig=
	fmap M.fromList $
	forM (M.toList embeddablesConfig) $ \(name, configFile) ->
	do
		loader' <-
			maybe (throwError $ "embeddable \"" ++ name ++ "\" not found") return $
				M.lookup name embeddableLoadersByName
		return (name, loader' configFile)

loadMainPlugins ::
	forall m .
	(MonadIO m, MonadError String m) =>
	M.Map MainPluginName MainLoaderContainer
	-> M.Map EmbeddableName (Yaml.Value -> m EmbeddableStateCont)
	-> MainPluginsConfig -> m [(URI, MainPluginStateCont, Embeddables)]
loadMainPlugins mainPluginsByName embeddableLoaders config =
	forM (M.toList config) $ \(name, MainPluginConfig{..}) ->
	do
		liftIO $ putStrLn $ concat ["loading plugin \"", name, "\"..." ]
		MainLoaderContainer mainLoader <-
			maybe (throwError $ "main plugin \"" ++ name ++ "\" not found") return $
			M.lookup name mainPluginsByName :: m MainLoaderContainer
		(mainPlugin, initState, embeddableParams) <- mainLoader mainPlugin_configFile
		--liftIO $ putStrLn $ concat ["embeddableParams: ", show embeddableParams]
		embeddables <-
			fmap M.fromList $
			forM (M.toList embeddableParams) $ \(instanceId, (embeddableName, params)) ->
				do
					embeddableLoader <-
						maybe (throwError $ "embeddable \"" ++ embeddableName ++ "\" not found") return $
						M.lookup embeddableName embeddableLoaders
						:: m (Yaml.Value -> m EmbeddableStateCont)
					embeddable <- embeddableLoader params :: m EmbeddableStateCont
					return (instanceId, embeddable)
			-- :: m Embeddables
		--liftIO $ putStrLn $ "bla2"
		return $
			( mainPlugin_uri
			, MainPluginStateCont (mainPlugin, initState)
			, embeddables
			)

-- |redirect a request to corresponding plugin
requestToPlugins ::
	(MonadIO m, MonadError String m) =>
	URI -> Request -> St.StateT AllPlugins m (Maybe Resource)
requestToPlugins =
	requestToPlugins' runPlugin
	where
		runPlugin ::
			(MonadIO m, MonadError String m) =>
			MainPlugin state -> state -> Request -> St.StateT AllPlugins m (Maybe Resource, state)
		runPlugin plugin st req =
			St.runStateT `flip` st $ mainPlugin_answerReq plugin req 

requestToPlugins' ::
	(MonadIO m, MonadError String m) =>
	(forall state . MainPlugin state -> state -> request -> St.StateT AllPlugins m (res, state))
	-> URI -> request -> St.StateT AllPlugins m res
requestToPlugins' runReq prefix request =
	St.get >>= \plugins ->
	do
		pluginState <-
			maybe (throwError $ "no plugin with prefix " ++ fromURI prefix) return $
			M.lookup prefix (allPlugins_mainPlugins plugins)
		(res, newState) <-
			case pluginState of
				MainPluginStateCont (plugin, st) ->
					(\(res,s) -> (res, MainPluginStateCont (plugin, s))) <$>
					runReq plugin st request
		St.put $
			Lens.over allPlugins_mainPlugins_L
				(M.insert prefix newState)
				plugins
		return res 

-- |redirect a request to corresponding plugin
requestToEmbeddables ::
	(MonadIO m, MonadError String m) =>
	EmbeddableInstanceID -> Request -> St.StateT AllPlugins m (Maybe Resource)
requestToEmbeddables instanceId =
	requestToEmbeddables' runPlugin instanceId
	where
		runPlugin ::
			(MonadIO m, MonadError String m) =>
			Embeddable params state -> params -> state -> Request -> St.StateT AllPlugins m (Maybe Resource, state)
		runPlugin plugin params st req =
			St.runStateT `flip` st $ embeddable_answerReq plugin instanceId params req 

requestToEmbeddables' ::
	forall m request res .
	(MonadIO m, MonadError String m) =>
	(forall params state . Embeddable params state -> params -> state -> request -> St.StateT AllPlugins m (res, state))
	-> EmbeddableInstanceID -> request -> St.StateT AllPlugins m res
requestToEmbeddables' runReq prefix request =
	St.get >>= \plugins ->
	do
		(pluginState ::  EmbeddableStateCont) <-
			maybe (throwError $ "no embeddable with instanceId \"" ++ prefix ++ "\"") return $
			M.lookup prefix (allPlugins_embeddables plugins)
		(res, newState) <-
			case pluginState of
				EmbeddableStateCont (plugin, params, st) ->
					(\(res,s) -> (res, EmbeddableStateCont (plugin, params, s))) <$>
					runReq plugin params st request
		St.put $
			Lens.over allPlugins_embeddables_L
				(M.insert prefix newState)
				plugins
		return res 

-- |redirect a request to corresponding plugin
execEmbeddable ::
	(MonadIO m, MonadError String m) =>
	EmbeddableInstanceID -> St.StateT AllPlugins m Section
execEmbeddable instanceId =
	St.get >>= \plugins ->
	do
		state <-
			maybe (throwError $ "no embeddable instance for id \"" ++ instanceId ++ "\"") return $
			M.lookup instanceId (allPlugins_embeddables plugins)
		(res, newState) <-
			case state of
				EmbeddableStateCont (plugin, params, st) ->
					(\(res,s) -> (res, EmbeddableStateCont (plugin, params, s))) <$>
					runReq plugin st params
		St.put $
			Lens.over allPlugins_embeddables_L
				(M.insert instanceId newState)
				plugins
		return res 
	where
		runReq ::
			(MonadIO m, MonadError String m) =>
			Embeddable params state -> state -> params -> St.StateT AllPlugins m (Section, state)
		runReq plugin st params =
			St.runStateT `flip` st $ embeddable_answerInternalReq plugin instanceId params
