{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ProjDB(
	Config(..),
	ProjDB,
	loadState,
	newState,
	--load,
	store,
	allArtists, allProjects, allPersons,
{-
	Config(..),
	MusicListState(..),
	MusicList,
	OperationT,
	Entry(),
	loadState,
	newState,
	keyFromEntry,
--	runMLState,
	addEntry,
	delEntry,
-}
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

{-
data Config
	= Config {
		config_filename :: FilePath
	}
	deriving( Read, Show )

type OperationT m = StateT MusicList m

keyFromEntry :: Entry -> Key
keyFromEntry e =
	(entry_artist e, entry_title e)

{-
runMLState :: MonadIO m => Config -> MusicListState -> OperationT m a -> m a
runMLState cfg st op =
	let
		path = config_filename cfg
	in
	do
		musicList <- liftIO $ atomically $ readTVar (mlState_ml st)
		(val, newList) <- runStateT op musicList
		when (newList /= musicList) $
			do
				liftIO $ atomically $ writeTVar (mlState_ml st) newList
				liftIO $ store path newList
		return val
-}

type MusicListState = MusicList

addEntry :: MonadIO m => Entry -> OperationT m ()
addEntry e = modify $ ([e]++)

delEntry :: MonadIO m => Key -> OperationT m ()
delEntry key =
	modify $ filter ((/=key) . keyFromEntry)

newState :: (MonadIO m, MonadError String m) => m MusicListState
newState =
	return []
	{-
	liftIO $ atomically $
	MusicListState <$> newTVar []
	-}

loadState :: (MonadIO m, MonadError String m) => Config -> m MusicListState
loadState cfg =
	let
		path = config_filename cfg
	in
	do
		ma <- liftIO $ load path
		flip (either throwError) ma $
			return
			{-
			\list ->
			do
				liftIO $ atomically $
					MusicListState <$> newTVar list
			-}

load :: FilePath -> IO (Either String MusicList)
load filename =
	either (Left . show) Right
	<$> decodeFileEither filename

store :: FilePath -> MusicList -> IO ()
store filename musicList =
	encodeFile filename musicList
-}
