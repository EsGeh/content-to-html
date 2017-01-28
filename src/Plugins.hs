{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
module Plugins where

import WebDocumentStructure

import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Control.Monad.State as St
import qualified Data.Text as T
import qualified Data.Map as M


data Plugin state
	= Plugin {
		plugin_answerReq :: forall m . (MonadIO m, MonadError String m) => Request -> St.StateT state m Resource,
		plugin_descr :: T.Text
	}

type Loader state = forall m . (MonadIO m, MonadError String m) => FilePath -> m (Plugin state, state)

data LoaderCont = forall state . LoaderCont (Loader state)
data PluginStateCont = forall state . PluginStateCont (Plugin state, state)

type Request = (URI, Params)
type Params = M.Map T.Text T.Text
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
				PluginStateCont <$>
					loader pluginCfg

routeToPlugins ::
	(MonadIO m, MonadError String m) =>
	Plugins -> URI -> Request -> m (Resource, Plugins)
routeToPlugins plugins prefix request =
	do
		pluginState <- maybe (throwError $ "no plugin with prefix " ++ fromURI prefix) return $ M.lookup prefix plugins
		(res, newState) <-
			case pluginState of
				PluginStateCont (plugin, st) ->
					(\(res,s) -> (res, PluginStateCont (plugin, s))) <$>
					runPlugin plugin st request
		return (res, M.insert prefix newState plugins)

runPlugin ::
	(MonadIO m, MonadError String m) =>
	Plugin state -> state -> Request -> m (Resource, state)
runPlugin plugin st req =
	plugin_answerReq plugin req `St.runStateT` st

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

{-
data PluginInterface pluginState
	= PluginInterface {
		plugin_load :: forall m . (MonadIO m, MonadError String m) => FilePath -> m pluginState,
		plugin_answerReq :: forall m . (MonadIO m, MonadError String m) => pluginState -> Request -> m Resource,
		plugin_descr :: T.Text
	}

data PluginCont = forall state . PluginCont (PluginInterface state)

data PluginStateCont = forall state . PluginStateCont (PluginInterface state, state)

plugins :: M.Map PluginName (FilePath, PluginCont)
plugins =
	M.fromList $
	[ ("testPlugin", ("test_configFile", PluginCont $ PluginInterface test_load test_answer "descr"))
	]

loadPlugins ::
	(MonadIO m, MonadError String m) =>
	m [PluginStateCont]
loadPlugins =
	mapM `flip` M.toList plugins $ \(name, (pluginCfg, pluginCont)) ->
	case pluginCont of
		PluginCont plugin ->
			PluginStateCont <$>
			loadPlugin plugin pluginCfg

loadPlugin ::
	(MonadIO m, MonadError String m) =>
	PluginInterface st -> FilePath -> m (PluginInterface st, st)
loadPlugin plugin pluginCfg =
		do
			st <- plugin_load plugin pluginCfg
			return $
				(plugin, st)

redirReqToPlugin ::
	PluginInterface st -> FilePath -> m (PluginInterface st, st)
-}
