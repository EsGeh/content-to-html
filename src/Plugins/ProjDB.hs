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
import Plugins.ProjDB.DB
import qualified Types.Resource as Resource
import qualified Plugins.ProjDB.ToWebDoc as ToWebDoc
--import Types.WebDocument hiding( Request )
import qualified Types.WebDocument as WebDoc
import qualified Plugins
import Types.URI

import Data.Yaml
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T
import Data.List


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
	| FilterNot Filter
	| FieldName `FilterEq` T.Text
	deriving( Show, Read )

genSection ::
	(MonadIO m, MonadError String m) =>
	Request -> ReadDBT m WebDoc.Section
genSection r =
	-- ((liftIO $ putStrLn $ "request: " ++ show r) >>) $
	case r of
		Artists filterExpr ->
			ToWebDoc.artistsPage =<< (lift $ filterExprToFunc filterExpr)
		Projects filterExpr ->
			ToWebDoc.projectsPage =<< (lift $ filterExprToFunc filterExpr)

parseRequest ::
	(MonadIO m, MonadError String m) =>
	Resource.Request -> m Request
parseRequest req@(uri, params)
	| uri == toURI "artists" = Artists <$> parseFilterExpr params
	| uri == toURI "projects" = Projects <$> parseFilterExpr params
	| otherwise = 
		throwError $ "request not found: " ++ show req

parseFilterExpr ::
	(MonadIO m, MonadError String m) =>
	Resource.Params -> m Filter
parseFilterExpr = f . sortParams
	where
		f params =
			case params of
				("not",_): subExpr -> FilterNot <$> f subExpr
				(field, value):_ -> return $ (FieldName field) `FilterEq` value
				[] -> return $ FilterAll
				-- _ -> throwError $ "error: could not parse filter: " ++ show params
		sortParams =
			sortBy $ \x _ -> if fst x == "not" then LT else GT

filterExprToFunc ::
	forall m a .
	(MonadIO m, MonadError String m, ToJSON a) =>
	Filter -> m (a -> Bool)
filterExprToFunc = \case
	FilterAll -> return $ const True
	FilterNot subExpr ->
		(not .) <$> filterExprToFunc subExpr
	fieldName `FilterEq` val -> return $
		\a ->
			contains fieldName a val

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
