--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CMS(
	module CMS,
	module CMS.Types,
	module CMS.ToHtml
) where

import CMS.Types
import CMS.ToHtml

import Data.Yaml
import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Data.Map as M
import qualified System.Directory as Dir
import System.IO.Error
import System.FilePath.Posix
import Data.List
import qualified Data.Text as T


type Routes = M.Map FilePath Resource
type PageRoutes = M.Map FilePath Page
type FileRoutes = M.Map FilePath FileResInfo

routes_pages :: Routes -> PageRoutes
routes_pages =
	M.mapMaybe (\r -> case r of { PageResource page -> Just page; _ -> Nothing}) 

routes_files :: Routes -> FileRoutes
routes_files =
	M.mapMaybe (\r -> case r of { FileResource info -> Just info; _ -> Nothing}) 

data Resource
	= PageResource Page
	| FileResource FileResInfo

data FileResInfo
	= FileResInfo {
		fileRes_type :: T.Text,
		fileRes_file :: FilePath
	}

findPage ::
	(MonadError String m) => String -> Routes -> m Resource
findPage key routes =
	let mRes = M.lookup key routes in
		maybe
			(throwError $ concat ["could not find \"", key, "\"!"{-, " possible: ", show $ M.keys content-}])
			return
			mRes

load :: 
	(MonadIO m, MonadError String m) =>
	[FilePath] -> [FileResInfo]
	-> m Routes
load pages otherRes =
	do
		static <-
			(fmap PageResource . M.unions) <$>
			mapM loadJson pages
		others <-
			fmap M.unions $
			forM otherRes $ \res ->
				fmap (FileResource . FileResInfo (fileRes_type res)) <$>
				createMap ("/" </>) <$>
				(getDirContents $
				fileRes_file res)
		return $
			M.union static others

loadJson ::
	(MonadIO m, MonadError String m, FromJSON a) =>
	FilePath -> m (M.Map FilePath a)
loadJson dir =
	do
		m <- createMap (\x -> "/" </> dropExtension x) <$> getDirContents dir
		mapM loadResource m

createMap :: (FilePath -> FilePath) -> [FilePath] -> M.Map FilePath FilePath
createMap toURI =
	M.fromList .
	(map $
		\path ->
			(toURI path, path)
	)

getDirContents ::
	(MonadIO m, MonadError String m) =>
	FilePath -> m [FilePath]
getDirContents dir =
	map (dir </>) <$> 
	filter (not . ("." `isPrefixOf`)) <$>
	do
		mRet<- liftIO (tryIOError $ Dir.getDirectoryContents dir)
		either
			(throwError . show)
			return
			mRet

loadResource ::
	(FromJSON res, MonadIO m, MonadError String m) =>
	FilePath -> m res
loadResource filename =
	do
		ma <- liftIO $ 
			either (Left . show) Right
			<$> decodeFileEither filename
		either
			(\e -> throwError $ concat ["error while loading \"", filename,"\": ", e])
			return
			ma
