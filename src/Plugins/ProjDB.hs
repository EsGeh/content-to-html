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
	= Artists (Maybe Filter)
	| Projects (Maybe Filter)
	deriving( Show, Read)

data Filter
	= Not Filter
	| FieldName `Equals` T.Text
	deriving( Show, Read)

genSection ::
	(MonadIO m, MonadError String m) =>
	Request -> ReadDBT m WebDoc.Section
genSection r =
	--((liftIO $ putStrLn $ "request: " ++ show r) >>) $
	case r of
		Artists mFilterExpr ->
			ToWebDoc.artistsPage $ maybe (const True) filterExprToFunc mFilterExpr
		Projects mFilterExpr ->
			ToWebDoc.projectsPage $ maybe (const True) filterExprToFunc mFilterExpr

filterExprToFunc ::
	forall a .
	(ToJSON a) =>
	Filter -> a -> Bool
filterExprToFunc = \case
	Not subExpr ->
		(not .) $ filterExprToFunc subExpr
	fieldName `Equals` val ->
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
