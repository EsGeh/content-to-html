{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Plugins.ProjDB(
	load,
	ProjDB(..),
) where

import Plugins.ProjDB.Types
import qualified Plugins.ProjDB.ToWebDoc as ToWebDoc
import WebDocumentStructure
import qualified Plugins

import Data.Yaml
import Control.Monad.State
import Control.Monad.Except


plugin :: Plugins.Plugin ProjDB
plugin = Plugins.Plugin {
	Plugins.plugin_answerReq = answer_req,
	Plugins.plugin_descr = "projDB"
}

load :: Plugins.Loader ProjDB
load =
	fmap (plugin, ) .
	loadState

answer_req ::
	(MonadIO m, MonadError String m) =>
	Plugins.Request -> StateT ProjDB m Resource
answer_req req =
	get >>= \db ->
		resFromReq db =<< requestFromParams req

resFromReq ::
	(MonadIO m, MonadError String m) =>
	ProjDB -> Request -> m Resource
resFromReq db req =
	case req of
		AllArtists ->
			return $ PageResource $ ToWebDoc.artistsPage "artists" (const True) db
		AllProjects ->
			return $ PageResource $ ToWebDoc.projectsPage "projects" (const True) db

data Request
	= AllArtists
	| AllProjects

requestFromParams ::
	(MonadIO m, MonadError String m) =>
	Plugins.Request -> m Request
requestFromParams (uri, _)
	| uri == toURI "artists" = return $ AllArtists
	| uri == toURI "projects" = return $ AllProjects
	| otherwise = 
		throwError "request not found!"
{-
	maybe (throwError "request not found!") return $
		(const AllArtists <$> M.lookup "artists" p)
		<|>
		(const AllArtists <$> M.lookup "projects" p)
-}

loadState :: (MonadIO m, MonadError String m) => FilePath -> m ProjDB
loadState cfg =
	fmap projDBFromEntries $
	either (throwError . show) return =<< liftIO (decodeFileEither cfg)

{-
newState :: (MonadIO m, MonadError String m) => m ProjDB
newState =
	return $ projDBDef

store :: FilePath -> ProjDB -> IO ()
store filename musicList =
	encodeFile filename $ projDBToEntries musicList
-}
