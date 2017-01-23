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
import Data.Maybe
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
	deriving( Show, Read )

data FileResInfo
	= FileResInfo {
		fileRes_type :: T.Text,
		fileRes_file :: FilePath
	}
	deriving( Show, Read )

findPage ::
	(MonadError String m) => String -> Routes -> m Resource
findPage key routes =
	let mRes = M.lookup key routes in
		maybe
			(throwError $ concat ["could not find \"", key, "\"!"{-, " possible: ", show $ M.keys content-}])
			return
			mRes

newtype URI = URI { fromURI :: FilePath }
	deriving( Eq, Ord, Show, Read )
newtype ResType = ResType { fromResType :: T.Text }

loadFilesInDir ::
	forall m .
	(MonadIO m, MonadError String m) =>
	(FilePath -> Maybe (URI, ResType))
	-> FilePath -> m (M.Map FilePath Resource)
loadFilesInDir calcResParams dirPath =
	loadFilesInDir' `flip` dirPath $ \path ->
	return $ do
		(uri, resType) <- calcResParams path
		return $
			( URI $ "/" </> fromURI uri
			, FileResource $
				FileResInfo{
					fileRes_type = fromResType resType,
					fileRes_file = dirPath </> path 
				}
			)

loadYamlInDir ::
	forall m .
	(MonadIO m, MonadError String m) =>
	(FilePath -> Maybe FilePath)
	-> FilePath -> m (M.Map FilePath Resource)
loadYamlInDir toURI dirPath =
	loadFilesInDir' `flip` dirPath $ \path ->
		maybe (return $ Nothing) `flip` toURI path $ \uri ->
				Just <$>
				(URI $ "/" </> uri,) <$>
				PageResource <$>
				(loadYaml $ dirPath </> path)

loadFilesInDir' ::
	forall m .
	(MonadIO m, MonadError String m) =>
	(FilePath -> m (Maybe (URI, Resource)))
	-> FilePath -> m (M.Map FilePath Resource)
loadFilesInDir' calcRes dirPath =
	calc =<< (ls dirPath :: m [FilePath])
	where
		calc :: [FilePath] -> m Routes
		calc paths = 
			(M.fromList . map (mapFst fromURI) . catMaybes) <$>
			mapM calcRes paths

mapFst f (a,b) = (f a, b)

loadYaml ::
	(FromJSON res, MonadIO m, MonadError String m) =>
	FilePath -> m res
loadYaml filename =
	do
		ma <- liftIO $ 
			either (Left . show) Right
			<$> decodeFileEither filename
		either
			(\e -> throwError $ concat ["error while loading \"", filename,"\": ", e])
			return
			ma

ls ::
	(MonadIO m, MonadError String m) =>
	FilePath -> m [FilePath]
ls dir =
	-- map (dir </>) <$> 
	filter (not . ("." `isPrefixOf`)) <$>
	do
		mRet<- liftIO (tryIOError $ Dir.getDirectoryContents dir)
		either
			(throwError . show)
			return
			mRet
