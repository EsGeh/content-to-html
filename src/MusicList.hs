{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MusicList(
	Config(..),
	MusicListState(..),
	MusicList,
	OperationT,
	Entry(),
	loadState,
	newState,
	keyFromEntry,
	runMLState,
	addEntry,
	delEntry,
) where

import MusicList.Types

import Data.Yaml
import Control.Monad.State
import Data.Maybe
import Control.Monad.Except
import Control.Concurrent.STM

data Config
	= Config {
		config_filename :: FilePath
	}
	deriving( Read, Show )

type OperationT m = StateT MusicList m

keyFromEntry e =
	(entry_artist e, entry_title e)

runMLState :: MonadIO m => Config -> MusicListState -> OperationT m a -> m a
runMLState cfg state op =
	let
		path = config_filename cfg
	in
	do
		musicList <- liftIO $ atomically $ readTVar (mlState_ml state)
		(val, newList) <- runStateT op musicList
		when (newList /= musicList) $
			do
				liftIO $ atomically $ writeTVar (mlState_ml state) newList
				liftIO $ store path newList
		return val

addEntry :: MonadIO m => Entry -> OperationT m ()
addEntry e = modify $ ([e]++)

delEntry :: MonadIO m => Key -> OperationT m ()
delEntry key =
	modify $ filter ((/=key) . keyFromEntry)

newState :: (MonadIO m, MonadError String m) => m MusicListState
newState =
	liftIO $ atomically $
		MusicListState <$> newTVar []

loadState :: (MonadIO m, MonadError String m) => Config -> m MusicListState
loadState cfg =
	let
		path = config_filename cfg
	in
	do
		ma <- liftIO $ load path
		flip (either throwError) ma $
			\list ->
			do
				liftIO $ atomically $
					MusicListState <$> newTVar list

load :: FilePath -> IO (Either String MusicList)
load filename =
	either (Left . show) Right
	<$> decodeFileEither filename

store :: FilePath -> MusicList -> IO ()
store filename musicList =
	encodeFile filename musicList
