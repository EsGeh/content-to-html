{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ProjDB(
	Config(..),
	ProjDB(..),
	loadState,
	newState,
	store,
	module ProjDB.Types
) where

import ProjDB.Types

import Data.Yaml
import Control.Monad.State
import Control.Monad.Except

newState :: (MonadIO m, MonadError String m) => m ProjDB
newState =
	return $ projDBDef

loadState :: (MonadIO m, MonadError String m) => Config -> m ProjDB
loadState cfg =
	(either throwError return) =<<
	(liftIO $ load $ config_filename cfg)

load :: FilePath -> IO (Either String ProjDB)
load filename =
	fmap (fmap $ projDBFromEntries) $
	either (Left . show) Right
	<$> decodeFileEither filename

store :: FilePath -> ProjDB -> IO ()
store filename musicList =
	encodeFile filename $ projDBToEntries musicList

data Config
	= Config {
		config_filename :: FilePath
	}
	deriving( Read, Show )
