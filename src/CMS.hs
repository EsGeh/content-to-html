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


type Content = M.Map FilePath Page

findPage ::
	(MonadIO m, MonadError String m) =>
	FilePath -> Content -> m Page
findPage key content =
	let mRes = M.lookup key content in
		maybe
			(throwError $ concat ["could not find \"", key, "\""])
			return
			mRes

load ::
	(MonadIO m, MonadError String m) =>
	Config -> m Content
load cfg =
	let dir = config_path cfg in
	liftM M.fromList $
	do
		files <- map (dir </>) <$> getDirContents dir
		forM files $ \path ->
			do
				ma <- liftIO $ loadPage path
				either
					(\e -> throwError $ concat ["error while loading \"", path,"\": ", e])
					(return . ("/" </> dropExtension path,))
					ma

getDirContents ::
	(MonadIO m, MonadError String m) =>
	FilePath -> m [FilePath]
getDirContents dir =
	filter (not . ("." `isPrefixOf`)) <$>
	do
		mRet<- liftIO (tryIOError $ Dir.getDirectoryContents dir)
		either
			(throwError . show)
			return
			mRet

loadPage :: FilePath -> IO (Either String Page)
loadPage filename =
	either (Left . show) Right
	<$> decodeFileEither filename

storePage :: FilePath -> Article -> IO ()
storePage =
	encodeFile

data Config
	= Config {
		config_path :: FilePath
	}
	deriving( Show, Read )
