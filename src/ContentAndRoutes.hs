{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module ContentAndRoutes(
	module ContentAndRoutes,
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
import Control.Applicative

import GHC.Generics


-----------------------------------
-- types
-----------------------------------

--type ContentWithPos = (Content, URI)

{- |this type represents a hierarchy of displayable content.
	It might be used as a basis for a site navigation (menu)
-}
type Content = [ContentEntry]

data ContentEntry
	= ContentEntry {
		content_caption :: T.Text,
		content_subEntries :: Either URI Content
	}
	deriving( Show, Read, Eq, Ord, Generic )

loadContent :: 
	(MonadIO m, MonadError String m) =>
	FilePath -> m Content
loadContent = loadYaml

type Routes = M.Map URI Resource
type PageRoutes = M.Map URI Page
type FileRoutes = M.Map URI FileResInfo

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
	deriving( Eq, Ord, Show, Read, Generic )
newtype ResType = ResType { fromResType :: T.Text }
	deriving( Eq, Ord, Show, Read )

toURI :: FilePath -> URI
toURI =
	URI . normalizeURI
	where
		normalizeURI = ("/" </>)

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

instance FromJSON ContentEntry where
	parseJSON (Object x) =
		ContentEntry <$>
			x .: "caption" <*> (
			(fmap Left $ x .: "uri")
			<|>
			(fmap Right $ x .: "sub")
			)
	parseJSON _ = mempty

instance FromJSON URI where
	parseJSON = (toURI <$>) . parseJSON

instance ToJSON URI where
	toJSON = toJSON . fromURI
