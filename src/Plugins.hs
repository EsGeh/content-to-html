{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
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


data AllPlugins
	= AllPlugins{
		allPlugins_mainPlugins :: MainPlugins,
		allPlugins_embeddables :: Embeddables
	}

-- |run with a specific plugin type 'Plugin state'
type RunReqT state m a =
	St.StateT state (St.StateT AllPlugins m) a

type MainPlugins = M.Map URI MainPluginStateCont

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


data Embeddable ctxtConfig state
	= Embeddable {

		-- |answering an external request, optionally changing the view:
		embeddable_answerReq ::
			forall m . (MonadIO m, MonadError String m) =>
			EmbeddableInstanceID
			-> ctxtConfig
			-> Request -> RunReqT state m (Maybe Resource),
			
		-- |answering a request from an other plugin
		embeddable_answerInternalReq ::
			forall m . (MonadIO m, MonadError String m) =>
			EmbeddableInstanceID
			-> ctxtConfig
			-> RunReqT state m Section,

		embeddable_descr :: T.Text

	}

defaultEmbeddable :: Embeddable ctxtConfig state
defaultEmbeddable =
	Embeddable {
		embeddable_answerInternalReq = \_ _ -> throwError "internal requests are not allowed!",
		embeddable_answerReq = \_ _ _ -> throwError "requests not allowed!",
		Plugins.embeddable_descr = "defaultPlugin"
	}

type EmbeddableLoader m =
	Yaml.Value -> m EmbeddableStateCont

-- |container for a plugin bundled with its current state
data EmbeddableStateCont =
	forall ctxtConfig state . EmbeddableStateCont (Embeddable ctxtConfig state, ctxtConfig, state)

type Embeddables = M.Map EmbeddableInstanceID EmbeddableStateCont

type EmbeddableInstanceID = String

type EmbeddableName = String


$(Lens.makeLensesWith lensRules' ''AllPlugins)

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

{-
-------------------------------
-- use plugins:
-------------------------------

-- |load all plugins
loadPlugins ::
	(MonadIO m, MonadError String m) =>
	PluginsLoadParams
	-> m Plugins
loadPlugins pluginsLoadParams =
	fmap M.fromList $
	mapM `flip` M.toList pluginsLoadParams $ \(pref, (pluginCfg, loaderCont)) ->
		(pref, ) <$>
		case loaderCont of
			LoaderCont loader ->
				do
					liftIO $ putStrLn $ "registering plugin at prefix: " ++ fromURI pref
					PluginStateCont <$>
						loader pluginCfg

-- example plugin:

data TestState = TestState

test_load :: forall ctxtConfig m . (MonadIO m, MonadError String m) => FilePath -> m (Plugin ctxtConfig TestState, TestState)
test_load path =
	do
		liftIO $ putStrLn $ "test: loading " ++ path
		return $
			(,TestState)
			defPlugin {
				plugin_answerReq = test_answer,
				plugin_descr = "test description"
			}

test_answer ::
	(MonadIO m, MonadError String m) =>
	Request -> m (Maybe Resource)
test_answer req =
	do
		liftIO $ putStrLn $ "test: anwswering " ++ show req
		throwError "could not answer"
-}
