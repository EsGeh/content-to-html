{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Lib(
	Config(..),
	ProjDBConfig, mlConfig,
	DirConfig(..), defDirConfig,
	runHomepage
) where

import qualified ContentAndRoutes as CMS
import qualified WebDocumentStructure as WebDocs
import qualified ProjDB
import qualified ProjDB.ToWebDoc -- as .Html
import qualified Html

import Web.Spock.Safe
import Lucid
import Data.Monoid
import Control.Monad.Except
import qualified Data.Text as T

type ProjDBConfig = ProjDB.Config

data Config
	= Config {
		config_port :: Int,
		config_sharedDirs :: [DirConfig],
		config_projDB :: ProjDB.Config,
		config_userCSS :: Maybe FilePath,
		config_content :: FilePath
	}
	deriving (Show, Read)

data DirConfig
	= DirConfig {
		dirConfig_path :: FilePath,
		dirConfig_uriPrefix :: FilePath
	}
	deriving( Show, Read )

type RoutesM = SpockM DBConn Session GlobalState

type DBConn = ()
type Session = ()
type GlobalState = ()

initState :: GlobalState
initState = ()

defDirConfig :: FilePath -> DirConfig
defDirConfig path = DirConfig path path

runHomepage :: Config -> IO ()
runHomepage Config{..} =
	handleErrors' $
	do
		db <- ProjDB.loadState config_projDB
		sharedData <-
			(loadSharedData db config_sharedDirs `catchError` \e -> throwError ("error while loading sharedData: " ++ e))
		contentTree <- CMS.loadContent config_content
		liftIO $ runSpock config_port $
				spock (spockCfg $ initState) $
				spockRoutes sharedData contentTree (CMS.toURI <$> config_userCSS)
	where
		spockCfg initState' =
			defaultSpockCfg () PCNoDatabase initState'
		handleErrors' x =
			runExceptT x
			>>= either putStrLn return

spockRoutes :: CMS.Routes -> CMS.Content -> Maybe CMS.URI -> RoutesM ()
spockRoutes routes content mUserCss =
	hookAny GET $ (. calcRouteKey) $ \path ->
		handleErrors $
		do
			resource <- CMS.findPage path routes
			case resource of
				CMS.PageResource page ->
					(lift . html . Html.renderPage . fullPage mUserCss content) page
				CMS.FileResource CMS.FileResInfo{..} ->
					lift $ file (CMS.fromResType $ fileRes_type) $ fileRes_file
	where
		calcRouteKey r = CMS.toURI (T.unpack $ T.intercalate "/" r)

loadSharedData ::
	(MonadIO m, MonadError String m) =>
	ProjDB.ProjDB -> [DirConfig] -> m CMS.Routes
loadSharedData db sharedDirs =
	--((\x -> do{ liftIO $ print x; return x} ) =<<) $
	fmap (
		(CMS.addRoute (CMS.toURI "/content/artists") $ CMS.PageResource $
			ProjDB.ToWebDoc.artistsPage "artists I like" `flip` db $ (/="Samuel Gfrörer")
		)
		.
		(CMS.addRoute (CMS.toURI "/content/ownProjects") $ CMS.PageResource $
			ProjDB.ToWebDoc.projectsPage "own projects" `flip` db $
				(\p -> "Samuel Gfrörer" `elem` ProjDB.project_artist p)
		)
	) $
	fmap CMS.combineRoutes $
	forM sharedDirs $ \DirConfig{..} ->
		CMS.loadFilesInDir `flip` dirConfig_path $
		CMS.defLoadDirInfo dirConfig_uriPrefix dirConfig_path

mlConfig :: FilePath -> ProjDBConfig
mlConfig = ProjDB.Config

fullPage :: Maybe CMS.URI -> CMS.Content -> WebDocs.Page -> Html ()
fullPage mUserCss content page =
	Html.basePage mUserCss (WebDocs.page_title page) $
	Html.nav content
	<>
	WebDocs.pageToHtml page

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
