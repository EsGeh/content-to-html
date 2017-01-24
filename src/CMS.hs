--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CMS(
	module CMS,
) where

import WebDocumentStructure.Types
--import WebDocumentStructure.ToHtml
import Utils( mapFst )

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

-----------------------------------
-- create a Routes object:
-----------------------------------

combineRoutes ::
	[Routes] -> Routes
combineRoutes =
	M.unions

addRoute :: FilePath -> Resource -> Routes -> Routes
addRoute uri res =
	M.insert uri res

loadFilesInDir ::
	forall m .
	(MonadIO m, MonadError String m) =>
	(FilePath -> m (Maybe (URI, Resource)))
	-> FilePath -> m (M.Map FilePath Resource)
loadFilesInDir calcRes dirPath =
	calc =<< (ls dirPath :: m [FilePath])
	where
		calc :: [FilePath] -> m Routes
		calc paths = 
			(M.fromList . map (mapFst fromURI) . catMaybes) <$>
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
	(MonadError String m) => String -> Routes -> m Resource
findPage key routes =
	let mRes = M.lookup key routes in
		maybe
			(throwError $ concat ["could not find \"", key, "\"!"{-, " possible: ", show $ M.keys content-}])
			return
			mRes

data Resource
	= PageResource Page
	| FileResource FileResInfo
	deriving( Show, Read )

data FileResInfo
	= FileResInfo {
		fileRes_type :: ResType,
		fileRes_file :: FilePath
	}
	deriving( Show, Read )

newtype URI = URI { fromURI :: FilePath }
	deriving( Eq, Ord, Show, Read )
newtype ResType = ResType { fromResType :: T.Text }
	deriving( Eq, Ord, Show, Read )

defLoadDirInfo :: FilePath -> FilePath -> FilePath -> Maybe (URI, Resource)
defLoadDirInfo uriPrefix dir path =
	case takeExtension path of
		".mp3" -> Just $
			( CMS.URI $ uriPrefix </> path
			, FileResource $ defResource { fileRes_type = CMS.ResType $ "audio/mpeg" }
			)
		".pdf" -> Just $
			( CMS.URI $ uriPrefix </> path
			, FileResource $ defResource { fileRes_type = CMS.ResType $ "application/pdf" }
			)
		".css" -> Just $
			( CMS.URI $ uriPrefix </> path
			, FileResource $ defResource { fileRes_type = CMS.ResType $ "style/css" }
			)
		_ -> Just $
			( CMS.URI $ uriPrefix </> path
			, FileResource $ defResource
			)
	where
		defResource =
			FileResInfo {
				fileRes_type = CMS.ResType $ "unknown",
				fileRes_file = dir </> path
			}

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
