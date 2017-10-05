{-# LANGUAGE FlexibleContexts #-}
module Utils.Yaml(
	module Utils.Yaml,
	module Data.Yaml,
	module Data.Aeson,
) where

import Data.Yaml
import Data.Aeson hiding ( encode, decode )
import Control.Monad.IO.Class
import Control.Monad.Except

loadYaml ::
	(FromJSON res, MonadIO m, MonadError String m) =>
	FilePath -> m res
loadYaml filename =
	do
		ma <- liftIO $ 
			either (Left . show) Right
			<$> decodeFileEither filename
		either
			(\e -> throwError $ concat ["error while loading \"", filename,"\": ", e])
			return
			ma
