{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib(
	Config(..),
	MLConfig, mlConfig,
	ContentConfig, contentConfig,
	runHomepage
) where

import qualified CMS
import qualified MusicList as ML
import RoutesMonad
import qualified MusicList.Html as ML.Html
import RenderPage
import qualified Html

import Web.Spock.Safe
import Lucid
import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Except
import qualified Control.Monad.State as State
import Control.Applicative
import qualified Data.Text as T
import System.FilePath.Posix

type MLConfig = ML.Config
type ContentConfig = CMS.Config

mlConfig :: FilePath -> MLConfig
mlConfig = ML.Config

contentConfig :: FilePath -> FilePath -> FilePath -> ContentConfig
contentConfig = CMS.Config

data Config
	= Config {
		config_port :: Int,
		config_content :: CMS.Config,
		config_musicList :: ML.Config
	}
	deriving (Show, Read)

runHomepage :: Config -> IO ()
runHomepage conf =
	let
		port = config_port conf
		contentCfg = config_content conf
	in
		handleErrors $
		do
			mlState <- ML.loadState (config_musicList conf) <|> ML.newState
			content <-
				(CMS.load contentCfg `catchError` \e -> throwError ("error while loading content: " ++ e))
			liftIO $ runSpock port $
					spock (spockCfg $ GlobalState content mlState) $
					runRoutes (routes conf)
	where
		spockCfg initState =
			defaultSpockCfg () PCNoDatabase initState
		handleErrors x =
			runExceptT x
			>>= either putStrLn return

runRoutes ::
	RoutesM GlobalState () -> SpockM () () GlobalState ()
runRoutes =
	prehook $
	initRoutes <$> getState

routes :: Config -> RoutesM GlobalState ()
routes cfg = 
	do
		subRoutes "/content" (CMS.state_pages . globState_cms) $
			contentRoutes cfg
		subRoutes "/css" (CMS.state_stylesheets . globState_cms) $
			resourceRoutes "style/css"
		subRoutes "/data" (CMS.state_data . globState_cms) $
			resourceRoutes "audio/mpeg"

contentRoutes :: Config -> RoutesM (CMS.Content CMS.Page) ()
contentRoutes cfg =
	do
		methodGetVar "/" $ \(_ :: FilePath) ->
			handleErrors $
			do
				doc <- lift $ getRoute
				content <- lift $ getCtx
				page <-
					(either throwError return =<<) $
					runExceptT $
					CMS.findPage doc content
				(lift . html . renderPage . fullPage content doc) page
		where
			handleErrors x =
				runExceptT x
				>>=
				either
					(\e -> text $ T.pack $ "404: resource not found. error: " ++ e)
					return

resourceRoutes :: T.Text -> RoutesM (CMS.Content FilePath) ()
resourceRoutes mediaType =
	do
		methodGetVar "/" $ \(_ :: FilePath) ->
			handleErrors $
			do
				requested <- lift $ getRoute
				content <- lift $ getCtx
				page <-
					(either throwError return =<<) $
					runExceptT $
					CMS.findPage requested content
				lift $ file mediaType $ page
		where
			handleErrors x =
				runExceptT x
				>>=
				either
					(\e -> text $ T.pack $ "404: resource not found. error: " ++ e)
					return

fullPage :: CMS.Content CMS.Page -> FilePath -> CMS.Page -> Html ()
fullPage content route page =
	Html.basePage (CMS.page_title page) $
	(Html.nav $
		Html.calcNavLinks route $
		map (mapSnd CMS.page_title) $
		M.toList $
		content
	)
	<>
	CMS.pageToHtml page

mapSnd f (a,b) = (a, f b)

{-
routeList :: [(T.Text, T.Text)]
routeList =
	[ route "general info" "/"
	, route "music list" "/musicList"
	]
	where
		route = (,)

routes :: Config -> RoutesM GlobalState ()
routes cfg = 
	do
		subRoutes "/content" globState_content $
			contentRoutes cfg
		{-
		subRoutes "musicList" globState_musicList $
			musicListRoutes cfg
		-}

fullPage title content =
	do
		currentRoute <- getRoute
		return $ Html.basePage title $
			(Html.nav $
				Html.calcNavLinks (T.pack currentRoute) routeList
			)
			<> content

contentRoutes :: Config -> RoutesM CMS.Content ()
contentRoutes cfg =
	methodGet "/" $
		(html . renderPage) =<<  (fullPage "test" . CMS.articleToHtml) =<< getCtx

musicListRoutes :: Config -> RoutesM ML.MusicListState ()
musicListRoutes cfg =
	do
		methodGet "/" $
			{-
			(html . renderPage) =<<
			(fullPage "favourite music" . CMS.articleToHtml . CMS.Article "favourite music" . ML.Html.toSections . ML.mlState_ml) =<< getCtx
			-}
			withMusicList cfg $
			do
				ml <- State.get
				lift $
					(html . renderPage) =<<
					(fullPage "test" . CMS.articleToHtml . CMS.Article "bla" . ML.Html.toSections) =<<
					return ml
		methodGet "addEntry" $
			html $ renderPage $
			ML.Html.addEntry "/musicList/addEntry"
		methodPost "addEntry" $
			withMusicList cfg $
			do
				ML.addEntry =<< ML.Html.readAddEntryParams (lift . param')
				(liftIO . print) =<< State.get

withMusicList ::
	Config -> ML.OperationT (ActionM ML.MusicListState) a
	-> ActionM ML.MusicListState a
withMusicList cfg rest =
	do
		mlState <- getCtx
		ML.runMLState (config_musicList cfg) mlState $ 
			rest
-}
