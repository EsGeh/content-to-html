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

data State =
	State {
		state_pages :: Content Page,
		state_stylesheets :: Content FilePath,
		state_data :: Content FilePath
		{-
		state_stylesheets :: [FilePath],
		state_data :: [FilePath]
		-}
	}

type Content res = M.Map FilePath res

data Config
	= Config {
		config_pagesDir :: FilePath,
		config_cssDir :: FilePath,
		config_dataDir :: FilePath
	}
	deriving( Show, Read )

findPage ::
	(MonadError String m) => String -> Content res -> m res
findPage key content =
	let mRes = M.lookup key content in
		maybe
			(throwError $ concat ["could not find \"", key, "\"!"{-, " possible: ", show $ M.keys content-}])
			return
			mRes

load :: 
	(MonadIO m, MonadError String m) =>
	Config -> m State
load cfg =
	State <$>
		loadJson (config_pagesDir cfg)
		<*> (createMap ("/" </>) <$> getDirContents (config_cssDir cfg))
		<*> (createMap ("/" </>) <$> getDirContents (config_dataDir cfg))

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

{-
loadJson ::
	(FromJSON res, MonadIO m, MonadError String m) =>
	FilePath -> m (Content res)
loadJson dir =
	liftM M.fromList $
	do
		files <- getDirContents dir
		forM files $ \path ->
			("/" </> dropExtension path,) <$>
			loadResource path
-}

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

{-
loadResource ::
	FromJSON res =>
	FilePath -> IO (Either String res)
loadResource filename =
	either (Left . show) Right
	<$> decodeFileEither filename
-}

storePage :: FilePath -> Article -> IO ()
storePage =
	encodeFile
