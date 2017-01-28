{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Plugins.HierarchicWebsite where

import WebDocumentStructure
import Plugins

import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified System.Directory as Dir
import System.IO.Error
import System.FilePath.Posix
import Data.List
import Data.Maybe

data RoutesState = 
	RoutesState {
		content :: Content,
		routes :: Routes,
		userCss :: Maybe URI
	}
	deriving( Show, Read )

data Config
	= Config {
		config_sharedDirs :: [DirConfig],
		config_userCSS :: Maybe URI,
		config_content :: FilePath
	}
	deriving (Generic, Show, Read)

plugin :: Plugins.Plugin RoutesState
plugin = Plugin {
	plugin_answerReq = answer_req,
	plugin_descr = "hierarchic website"
}

load :: Plugins.Loader RoutesState
load configFile =
	(either (throwError . show) return =<< liftIO (decodeFileEither configFile)) >>= \Config{..} ->
	do
		liftIO $ putStrLn "loading website..."
		sharedData <-
			(loadSharedData config_sharedDirs `catchError` \e -> throwError ("error while loading sharedData: " ++ e))
		contentTree <- loadContent config_content
		liftIO $ putStrLn $ "content tree:" ++ show contentTree
		liftIO $ putStrLn $ "shared resources:" ++ show sharedData
		return $
			(plugin,) $
			RoutesState {
				content = contentTree,
				routes = sharedData,
				userCss = config_userCSS
			}

answer_req ::
	(MonadIO m, MonadError String m) =>
	Plugins.Request -> StateT RoutesState m Resource
answer_req (resKey,_) =
	get >>= \RoutesState{..} ->
		findPage resKey routes >>= \case
			PageResource page ->
				return $ FullPageResource $ PageWithNav (calcNav content) page
			otherRes -> return $ otherRes
	where

calcNav :: Content -> [NavEntry]
calcNav content =
	map `flip` content $ \e ->
		case content_subEntries e of
			Left uri -> NavEntry $ Link (content_caption e) (T.pack $ fromURI uri)
			Right subEntries -> NavCategory (content_caption e) (calcNav subEntries)

data DirConfig
	= DirConfig {
		dirConfig_path :: FilePath,
		dirConfig_uriPrefix :: FilePath
	}
	deriving( Show, Read, Generic )

defDirConfig :: FilePath -> DirConfig
defDirConfig path = DirConfig path path

{-
fullPage :: Maybe URI -> Content -> Page -> Html ()
fullPage mUserCss content page =
	Html.basePage mUserCss (page_title page) $
	Html.nav content
	<>
	pageToHtml page
-}

-----------------------------------
-- types
-----------------------------------

type Routes = M.Map URI Resource
type PageRoutes = M.Map URI Page
type FileRoutes = M.Map URI FileResInfo

loadSharedData ::
	(MonadIO m, MonadError String m) =>
	[DirConfig] -> m Routes
loadSharedData sharedDirs =
	--((\x -> do{ liftIO $ print x; return x} ) =<<) $
	fmap combineRoutes $
	forM sharedDirs $ \DirConfig{..} ->
		loadFilesInDir `flip` dirConfig_path $
		defLoadDirInfo dirConfig_uriPrefix dirConfig_path

-----------------------------------
-- create a Routes object:
-----------------------------------

combineRoutes ::
	[Routes] -> Routes
combineRoutes =
	M.unions

addRoute :: URI -> Resource -> Routes -> Routes
addRoute =
	M.insert

loadFilesInDir ::
	forall m .
	(MonadIO m, MonadError String m) =>
	(FilePath -> m (Maybe (URI, Resource)))
	-> FilePath -> m Routes
loadFilesInDir calcRes dirPath =
	calc =<< (ls dirPath :: m [FilePath])
	where
		calc :: [FilePath] -> m Routes
		calc paths = 
			(M.fromList . catMaybes) <$>
			mapM calcRes paths

-----------------------------------
-- filter/search routes:
-----------------------------------

routes_pages :: Routes -> PageRoutes
routes_pages =
	M.mapMaybe (\r -> case r of { PageResource page -> Just page; _ -> Nothing}) 

routes_files :: Routes -> FileRoutes
routes_files =
	M.mapMaybe (\r -> case r of { FileResource info -> Just info; _ -> Nothing}) 

findPage ::
	(MonadError String m) => URI -> Routes -> m Resource
findPage key routes =
	let mRes = M.lookup key routes in
		maybe
			(throwError $ concat ["could not find \"", fromURI key, "\"!"{-, " possible: ", show $ M.keys content-}])
			return
			mRes

defLoadDirInfo ::
	(MonadIO m, MonadError String m) =>
	FilePath -> FilePath -> FilePath -> m (Maybe (URI, Resource))
defLoadDirInfo uriPrefix dir path =
	case takeExtension path of
		".mp3" -> return $ Just $
			( URI $ "/" </> uriPrefix </> path
			, FileResource $ defResource { fileRes_type = ResType $ "audio/mpeg" }
			)
		".pdf" -> return $ Just $
			( URI $ "/" </> uriPrefix </> path
			, FileResource $ defResource { fileRes_type = ResType $ "application/pdf" }
			)
		".css" -> return $ Just $
			( URI $ "/" </> uriPrefix </> path
			, FileResource $ defResource { fileRes_type = ResType $ "style/css" }
			)
		".yaml" ->
			Just <$>
			( URI $ "/" </> uriPrefix </> dropExtension path, ) <$>
			PageResource <$>
			loadYaml (dir </> path)
		_ -> return $ Just $
			( URI $ "/" </> uriPrefix </> path
			, FileResource $ defResource
			)
	where
		defResource =
			FileResInfo {
				fileRes_type = ResType $ "unknown",
				fileRes_file = dir </> path
			}

-----------------------------------
-- utils:
-----------------------------------

ls ::
	(MonadIO m, MonadError String m) =>
	FilePath -> m [FilePath]
ls dir =
	-- map (dir </>) <$> 
	filter (not . ("." `isPrefixOf`)) <$>
	do
		mRet <- liftIO (tryIOError $ Dir.getDirectoryContents dir)
		either
			(throwError . show)
			return
			mRet

instance FromJSON Config where
instance ToJSON Config where
instance FromJSON DirConfig where
instance ToJSON DirConfig where
