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


{-
data FullSite
	= FullSite {
		fullSite_content :: ContentWithPos,
		fullSite_routes :: Routes
	}
	deriving( Show )

type ContentWithPos = (ContentTree, URI)

data ContentTree
	= ContentEntry ContentEntryInfo
	| ContentNode ContentTree
	deriving( Show )

data ContentEntryInfo
	= ContentEntryInfo {
		content_caption :: T.Text,
		content_uri :: URI
	}
	deriving( Show )
-}

type Routes = M.Map URI Resource
type PageRoutes = M.Map URI Page
type FileRoutes = M.Map URI FileResInfo

-----------------------------------
-- create a Routes object:
-----------------------------------

toURI :: FilePath -> URI
toURI =
	URI . normalizeURI
	where
		normalizeURI = ("/" </>)

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
	-> FilePath -> m (M.Map URI Resource)
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

defLoadDirInfo ::
	(MonadIO m, MonadError String m) =>
	FilePath -> FilePath -> FilePath -> m (Maybe (URI, Resource))
defLoadDirInfo uriPrefix dir path =
	case takeExtension path of
		".mp3" -> return $ Just $
			( CMS.URI $ "/" </> uriPrefix </> path
			, FileResource $ defResource { fileRes_type = CMS.ResType $ "audio/mpeg" }
			)
		".pdf" -> return $ Just $
			( CMS.URI $ "/" </> uriPrefix </> path
			, FileResource $ defResource { fileRes_type = CMS.ResType $ "application/pdf" }
			)
		".css" -> return $ Just $
			( CMS.URI $ "/" </> uriPrefix </> path
			, FileResource $ defResource { fileRes_type = CMS.ResType $ "style/css" }
			)
		".yaml" ->
			Just <$>
			( CMS.URI $ "/" </> uriPrefix </> dropExtension path, ) <$>
			PageResource <$>
			loadYaml (dir </> path)
		_ -> return $ Just $
			( CMS.URI $ "/" </> uriPrefix </> path
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
