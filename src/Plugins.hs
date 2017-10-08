{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
module Plugins where

import Types.Resource
import Types.WebDocument
import Types.URI

import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Control.Monad.State as St
import qualified Data.Text as T
import qualified Data.Map as M


type PluginSystemM m a =
	St.StateT Plugins m a

-- |run with a specific plugin type 'Plugin state'
type RunReqT state m a =
	St.StateT state (St.StateT Plugins m) a

-- |a "plugin" is something that can answer requests by returning a '.Resource'
data Plugin state
	= Plugin {
		-- |answering an external request, optionally changing the view:
		plugin_answerReq ::
			forall m . (MonadIO m, MonadError String m) =>
			Request -> RunReqT state m (Maybe Resource),
		-- |answering a request from an other plugin
		plugin_answerInternalReq ::
			forall m . (MonadIO m, MonadError String m) =>
			Request -> RunReqT state m Section,
		plugin_descr :: T.Text
	}
defPlugin :: Plugin state
defPlugin =
	Plugin{
		plugin_answerReq = \_ ->
			throwError "no requests allowed",
		plugin_answerInternalReq = \_ ->
			throwError "no internal requests allowed",
		plugin_descr = "no description available"
	}

type Plugins = M.Map URI PluginStateCont

-- |create a plugin and a corresponding initial state
type Loader state =
	forall m . (MonadIO m, MonadError String m) =>
	FilePath -> m (Plugin state, state)

type PluginsLoadParams = M.Map URI (FilePath, LoaderCont)

-- |container for a plugin loader
data LoaderCont =
	forall state . LoaderCont (Loader state)

-- |container for a plugin bundled with its current state
data PluginStateCont =
	forall state . PluginStateCont (Plugin state, state)

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

-- |redirect a request to corresponding plugin
requestToPlugins ::
	(MonadIO m, MonadError String m) =>
	URI -> Request -> St.StateT Plugins m (Maybe Resource)
requestToPlugins =
	requestToPlugins' runPlugin
	where
		runPlugin ::
			(MonadIO m, MonadError String m) =>
			Plugin state -> state -> Request -> St.StateT Plugins m (Maybe Resource, state)
		runPlugin plugin st req =
			St.runStateT `flip` st $ plugin_answerReq plugin req 

-- |redirect a request to corresponding plugin
requestToPluginsInternal ::
	(MonadIO m, MonadError String m) =>
	URI -> Request -> St.StateT Plugins m Section
requestToPluginsInternal =
	requestToPlugins' runPlugin
	where
		runPlugin ::
			(MonadIO m, MonadError String m) =>
			Plugin state -> state -> Request -> St.StateT Plugins m (Section, state)
		runPlugin plugin st req =
			St.runStateT `flip` st $ plugin_answerInternalReq plugin req 

requestToPlugins' ::
	(MonadIO m, MonadError String m) =>
	(forall state . Plugin state -> state -> Request -> St.StateT Plugins m (res, state))
	-> URI -> Request -> St.StateT Plugins m res
requestToPlugins' runReq prefix request =
	St.get >>= \plugins ->
	do
		pluginState <-
			maybe (throwError $ "no plugin with prefix " ++ fromURI prefix) return $
			M.lookup prefix plugins
		(res, newState) <-
			case pluginState of
				PluginStateCont (plugin, st) ->
					(\(res,s) -> (res, PluginStateCont (plugin, s))) <$>
					runReq plugin st request
		St.put $ M.insert prefix newState plugins
		return res 

-- example plugin:

data TestState = TestState

test_load :: forall m . (MonadIO m, MonadError String m) => FilePath -> m (Plugin TestState, TestState)
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
