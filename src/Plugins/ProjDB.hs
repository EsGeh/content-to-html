{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Plugins.ProjDB(
	load,
	ProjDB(..),
) where

import Plugins.ProjDB.Types
import qualified Plugins.ProjDB.ToWebDoc as ToWebDoc
import WebDocumentStructure hiding( Request )
import qualified WebDocumentStructure as WebDoc
import qualified Plugins
import Types

import Data.Yaml
import Control.Monad.State
import Control.Monad.Except


plugin :: Plugins.Plugin ProjDB
plugin = Plugins.defPlugin {
	-- Plugins.plugin_answerReq = 
	Plugins.plugin_answerInternalReq = \req -> get >>= \db -> genSection db =<< parseRequest req,
	Plugins.plugin_descr = "projDB"
}

load :: Plugins.Loader ProjDB
load =
	fmap (plugin, ) .
	loadState

data Request
	= AllArtists
	| AllProjects

parseRequest ::
	(MonadIO m, MonadError String m) =>
	WebDoc.Request -> m Request
parseRequest req@(uri, _)
	| uri == toURI "artists" = return $ AllArtists
	| uri == toURI "projects" = return $ AllProjects
	| otherwise = 
		throwError $ "request not found: " ++ show req

genSection ::
	(MonadIO m, MonadError String m) =>
	ProjDB -> Request -> m Section
genSection db = \case
	AllArtists ->
		return $ ToWebDoc.artistsPage (const True) db
	AllProjects ->
		return $ ToWebDoc.projectsPage (const True) db

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
