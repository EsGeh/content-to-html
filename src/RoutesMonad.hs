--{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE TypeFamilies #-}
module RoutesMonad where

import qualified MusicList as ML
import Web.Spock.Safe

import System.FilePath.Posix
import Control.Monad.IO.Class

import Data.String

--type RoutesM = SpockM () () GlobalState


type RoutesM ctx = SpockCtxT (RoutesCtx ctx) (WebStateM () () GlobalState)
--type RoutesM ctx = SpockM () () (RoutesCtx ctx)
-- == SpockCtxM () () () (RoutesCtx ctx)
-- == SpockCtxT () (WebStateM () () (RoutesCtx ctx))

--type SpockM conn sess st = SpockCtxM () conn sess st
--type SpockCtxM ctx conn sess st = SpockCtxT ctx (WebStateM conn sess st)

type ActionM ctx = SpockActionCtx (RoutesCtx ctx) () () GlobalState
-- == ActionCtxT (RoutesCtx ctx) (WebStateM () () (RoutesCtx ctx))

--type SpockActionCtx ctx conn sess st = ActionCtxT ctx (WebStateM conn sess st)
{-
	prehook :: forall m ctx ctx'. MonadIO m =>
		ActionCtxT ctx m ctx' ->
		SpockCtxT ctx' m () -> SpockCtxT ctx m ()
-}

data RoutesCtx ctx =
	RoutesCtx {
		routesCtx_ctx :: ctx,
		routesCtx_route :: String
	}

data GlobalState =
	GlobalState {
		globState_musicList :: ML.MusicListState
	}

initRoutes :: st -> RoutesCtx st
initRoutes st =
	RoutesCtx {
		routesCtx_ctx = st,
		routesCtx_route = "/"
	}

subRoutes ::
	String -> (ctx -> ctx') -> RoutesM ctx' () -> RoutesM ctx ()
subRoutes subroute calcSubCtx f =
	subcomponent (fromString subroute) $
	prehook (
		do
			ctx <- getCtx
			route <- getRoute
			--liftIO $ putStrLn $ concat ["subRoutes prehook: ", route, "</>", subroute]
			return $
				RoutesCtx {
					routesCtx_ctx = calcSubCtx ctx,
					routesCtx_route = route </> dropDrive subroute
				}
	) f

getCtx :: ActionM ctx ctx
getCtx =
	routesCtx_ctx <$> getContext

methodGet :: String -> ActionM ctx () -> RoutesM ctx ()
methodPost :: String -> ActionM ctx () -> RoutesM ctx ()

methodGet = method get
methodPost = method post

method ::
	IsString a =>
	(a -> ActionM ctx b -> t)
	-> String
	-> ActionM ctx b
	-> t
method meth route f =
	meth (fromString route) $
	do
		oldCtx <- getContext
		let oldRoute = routesCtx_route oldCtx
		--liftIO $ putStrLn $ concat $ ["methodGet: ", oldRoute, "</>", route]
		runInContext
			(oldCtx{ routesCtx_route = oldRoute </> dropDrive route }) $
			do
				(liftIO . putStrLn) =<< getRoute
			 	f

getRoute :: ActionM ctx String
getRoute =
	routesCtx_route <$> getContext
