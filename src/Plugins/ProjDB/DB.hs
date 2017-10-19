{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Plugins.ProjDB.DB where

import qualified Data.Text as T
import Control.Monad.Reader
import Data.Maybe

--import Data.Aeson
import qualified Utils.Yaml as Yaml
import qualified Utils.JSONOptions as Yaml
import qualified Data.Aeson.TH as Yaml
import qualified Data.HashMap.Lazy as HM


class
		(HasKey val key) =>
		DB db key val | db val -> key, db key -> val
	where
		dbLookup :: db -> key -> Maybe val
		dbKeys :: db -> [key]

class HasKey val key | val -> key where
	getKey :: val -> key

class FromKey a where
	fromKey :: a -> T.Text

class HasField cont field where
	getField :: FieldName -> cont -> Maybe field

newtype FieldName = FieldName { fromFieldName :: T.Text }
	deriving( Eq, Ord, Show, Read)

runReadDBT :: ReaderT db m a -> db -> m a
runReadDBT = runReaderT

lookupDB ::
	(Monad m, DB db key val) =>
	key -> ReaderT db m (Maybe val)
lookupDB key = ask >>=
	return . flip dbLookup key

select ::
	forall m db val key .
	(Monad m, DB db key val) =>
	(val -> Bool)
	-> ReaderT db m [val]
select cond = ask >>= \db ->
	return $
	filter cond $
	mapMaybe (dbLookup db) $
	dbKeys db

-- |test if `cont` has a `field` CONTAINING the value `val`
contains :: Yaml.ToJSON a => FieldName -> a -> T.Text -> Bool
contains field cont val =
	contains' $ Yaml.toJSON cont
	where
		contains' :: Yaml.Value -> Bool
		contains' (Yaml.Object m) =
			maybe False `flip` HM.lookup (fromFieldName field) m $ \case
				Yaml.String s -> s == val
				Yaml.Array a ->
					or $
					fmap `flip` a $ \case
						Yaml.String s -> s == val
						_ -> False
				_ -> False
		contains' _ = False

getFieldVal :: Yaml.ToJSON a => FieldName -> a -> Maybe T.Text
getFieldVal field cont =
	getFieldVal' $ Yaml.toJSON cont
	where
		getFieldVal' :: Yaml.Value -> Maybe T.Text
		getFieldVal' (Yaml.Object m) =
			HM.lookup (fromFieldName field) m >>= \case
				Yaml.String s -> Just s
				_ -> Nothing
		getFieldVal' _ = Nothing

instance Yaml.FromJSON FieldName where
	parseJSON = (FieldName <$>) . Yaml.parseJSON

instance Yaml.ToJSON FieldName where
	toJSON = Yaml.toJSON . fromFieldName
	--parseJSON = (FieldName <$>) . Yaml.parseJSON

{-
class CanContain a b where
	contains :: a -> b -> Bool

instance (Eq a) => CanContain a a where
	contains = (==)

instance (Eq a) => CanContain [a] a where
	contains l x = x `elem` l
-}

-- $(Yaml.deriveJSON Yaml.jsonOptions ''FieldName)
