{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Plugins.ProjDB(
	embeddable,
) where

import Plugins.ProjDB.Types
import Plugins.ProjDB.DB
import qualified Plugins.ProjDB.ToWebDoc as ToWebDoc
import qualified Types.WebDocument as WebDoc
import qualified Plugins
import Utils.Yaml
import Utils.JSONOptions
import Data.Aeson.TH

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Text as T


embeddable :: Plugins.Embeddable Request ProjectsState
embeddable = Plugins.defaultEmbeddable {
	Plugins.embeddable_answerInternalReq = \_ params ->
		get >>= \(ProjectsState db) ->
		runReadDBT `flip` db $
			genSection params,
	Plugins.embeddable_descr = "projDB"
}

newtype ProjectsState = ProjectsState { fromProjectsState :: ProjDB }

instance FromJSON ProjectsState where
	parseJSON =
		fmap (ProjectsState . projDBFromEntries) .
		parseJSON


data Request
	= Artists Filter
	| Projects Filter
	deriving( Show, Read)

data Filter
	= FilterAll
	| FilterNot Filter
	| FieldName `FilterEq` T.Text
	deriving( Show, Read)

genSection ::
	(MonadIO m, MonadError String m) =>
	Request -> ReadDBT m WebDoc.Section
genSection r =
	--((liftIO $ putStrLn $ "request: " ++ show r) >>) $
	case r of
		Artists filterExpr ->
			ToWebDoc.artistsPage =<< (lift $ filterExprToFunc filterExpr)
		Projects filterExpr ->
			ToWebDoc.projectsPage =<< (lift $ filterExprToFunc filterExpr)

filterExprToFunc ::
	forall m a .
	(MonadIO m, MonadError String m, ToJSON a) =>
	Filter -> m (a -> Bool)
filterExprToFunc = \case
	FilterAll -> return $ const True
	FilterNot subExpr ->
		(not .) <$> filterExprToFunc subExpr
	fieldName `FilterEq` val ->
		return $
		\a ->
			contains fieldName a val

{-
newState :: (MonadIO m, MonadError String m) => m ProjDB
newState =
	return $ projDBDef

store :: FilePath -> ProjDB -> IO ()
store filename musicList =
	encodeFile filename $ projDBToEntries musicList
-}

$(deriveJSON jsonOptions ''Request)
$(deriveJSON jsonOptions ''Filter)
