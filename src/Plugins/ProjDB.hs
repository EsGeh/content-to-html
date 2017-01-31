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
import qualified Data.Map as M
import qualified Data.Text as T


plugin :: Plugins.Plugin ProjDB
plugin = Plugins.defPlugin {
	-- Plugins.plugin_answerReq = 
	Plugins.plugin_answerInternalReq = \req ->
		get >>= \db ->
		runReadDBT `flip` db $
			genSection =<< parseRequest req,
	Plugins.plugin_descr = "projDB"
}

load :: Plugins.Loader ProjDB
load =
	fmap (plugin, ) .
	loadState

data Request
	= Artists Filter
	| Projects Filter
	deriving( Show, Read )

data Filter
	= FilterAll
	| FilterEq T.Text
	deriving( Show, Read )

genSection ::
	(MonadIO m, MonadError String m) =>
	Request -> ReadDBT m Section
genSection r =
	--((liftIO $ putStrLn $ "request: " ++ show r) >>) $
	case r of
		Artists filterExpr ->
			ToWebDoc.artistsPage $ filterExprToFunc filterExpr
		Projects filterExpr ->
			ToWebDoc.projectsPage $ filterExprToFunc filterExpr

parseRequest ::
	(MonadIO m, MonadError String m) =>
	WebDoc.Request -> m Request
parseRequest req@(uri, params)
	| uri == toURI "artists" = return $ Artists $ parseFilterExpr params
	| uri == toURI "projects" = return $ Projects $ parseFilterExpr params
	| otherwise = 
		throwError $ "request not found: " ++ show req

parseFilterExpr params =
	maybe FilterAll `flip` M.lookup "filter" params $ FilterEq

filterExprToFunc :: (HasKey a key, FromKey key) => Filter -> (a -> Bool)
filterExprToFunc = \case
	FilterAll -> const True
	FilterEq val -> (==val) . fromKey . key

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
