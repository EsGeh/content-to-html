{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
module Plugins where

import WebDocumentStructure
import Types

import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Control.Monad.State as St
--import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Map as M


data Plugin state
	= Plugin {
		plugin_answerReq ::
			forall m . (MonadIO m, MonadError String m) =>
			Request -> RunReqT state m Resource,
		plugin_descr :: T.Text
	}

type RunReqT state m a =
	St.StateT state (St.StateT Plugins m) a

type Loader state = forall m . (MonadIO m, MonadError String m) => FilePath -> m (Plugin state, state)

data LoaderCont = forall state . LoaderCont (Loader state)
data PluginStateCont = forall state . PluginStateCont (Plugin state, state)

type PluginName = String

type PluginsLoadParams = M.Map URI (FilePath, LoaderCont)
type Plugins = M.Map URI PluginStateCont

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

routeToPlugins ::
	(MonadIO m, MonadError String m) =>
	URI -> Request -> St.StateT Plugins m Resource
routeToPlugins prefix request =
	St.get >>= \plugins ->
	do
		pluginState <-
			maybe (throwError $ "no plugin with prefix " ++ fromURI prefix) return $
			M.lookup prefix plugins
		(res, newState) <-
			case pluginState of
				PluginStateCont (plugin, st) ->
					(\(res,s) -> (res, PluginStateCont (plugin, s))) <$>
					runPlugin plugin st request
		St.put $ M.insert prefix newState plugins
		return res 

runPlugin ::
	(MonadIO m, MonadError String m) =>
	Plugin state -> state -> Request -> St.StateT Plugins m (Resource, state)
runPlugin plugin st req =
	--((liftIO $ putStrLn $ "running plugin: " ++ show (plugin_descr plugin)) >>) $
	St.runStateT `flip` st $ plugin_answerReq plugin req 

-- test plugin:

data TestState = TestState

test_load :: forall m . (MonadIO m, MonadError String m) => FilePath -> m (Plugin TestState, TestState)
test_load path =
	do
		liftIO $ putStrLn $ "test: loading " ++ path
		return $
			(,TestState)
			Plugin {
				plugin_answerReq = test_answer,
				plugin_descr = "test description"
			}

test_answer ::
	(MonadIO m, MonadError String m) =>
	Request -> m Resource
test_answer req =
	do
		liftIO $ putStrLn $ "test: anwswering " ++ show req
		throwError "could not answer"
