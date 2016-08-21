{-# LANGUAGE OverloadedStrings #-}
module Lib(
	Config(..),
	MLConfig, mlConfig,
	runHomepage
) where

import qualified MusicList as ML
import RoutesMonad
import qualified MusicList.Html as ML.Html
import RenderPage
import qualified Html

import Web.Spock.Safe
import Data.Monoid
import Control.Monad.Except
import qualified Control.Monad.State as State
import Control.Applicative
import qualified Data.Text as T

type MLConfig = ML.Config

mlConfig :: FilePath -> MLConfig
mlConfig = ML.Config

data Config
	= Config {
		config_port :: Int,
		config_musicList :: ML.Config
	}
	deriving (Show, Read)


runHomepage :: Config -> IO ()
runHomepage conf =
	let
		port = config_port conf
	in
		handleErrors $
		(ML.loadState (config_musicList conf) <|> ML.newState)
		>>=
		(\mlState -> liftIO $ runSpock port $
				spock (spockCfg $ GlobalState mlState) $
				(prehook $ initRoutes <$> getState) (routes conf)
		)
	where
		spockCfg initState =
			defaultSpockCfg () PCNoDatabase initState
		handleErrors x =
			runExceptT x
			>>= either print return


routes :: Config -> RoutesM GlobalState ()
routes cfg = 
	do
		methodGet "/" $ infoPage
		subRoutes "musicList" globState_musicList $
			musicListRoutes cfg

infoPage :: ActionM ctx a
infoPage =
	(getRoute >>=) $ \route ->
	html $ renderPage $
		Html.basePage "general info"
		(Html.nav $
			Html.calcNavLinks (T.pack route) routeList
		)
		<>
		Html.info

withMusicList ::
	Config -> ML.OperationT (ActionM ML.MusicListState) a
	-> ActionM ML.MusicListState a
withMusicList cfg rest =
	do
		mlState <- getCtx
		ML.runMLState (config_musicList cfg) mlState $ 
			rest

musicListRoutes :: Config -> RoutesM ML.MusicListState ()
musicListRoutes cfg =
	do
		methodGet "/" $
			withMusicList cfg $
			do
				currentRoute <- lift $ getRoute
				ml <- State.get
				lift $ html $ renderPage $
					Html.basePage "musicList" $
					(Html.nav $
						Html.calcNavLinks (T.pack currentRoute) routeList
					)
					<>
					(Html.textContent "musicList" $
					mapM_ ML.Html.showEntry ml)
		methodGet "addEntry" $
			html $ renderPage $
			ML.Html.addEntry "/musicList/addEntry"
		methodPost "addEntry" $
			withMusicList cfg $
			do
				ML.addEntry =<< ML.Html.readAddEntryParams (lift . param')
				(liftIO . print) =<< State.get

routeList :: [(T.Text, T.Text)]
routeList =
	[ route "general info" "/"
	, route "music list" "/musicList"
	]
	where
		route = (,)
