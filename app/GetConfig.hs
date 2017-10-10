{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module GetConfig(
	getConfig
) where

import qualified Lib

import Options.Applicative
import Data.Monoid
-- import Control.Exception.Base
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Except
import System.Exit
--import qualified Data.Text as T


data Config
	= Config {
		config_port :: Int,
		config_pluginsConfig :: Lib.PluginsConfig,
		config_attributesConfig :: FilePath
	}
	deriving (Show, Read)


getConfig :: IO Lib.Config
getConfig =
	toLibConfig =<< execParser parserInfo

toLibConfig ::
	Config -> IO Lib.Config
toLibConfig Config{..} =
	do
		attributesCfg <-
			(either die return =<< ) $
			runExceptT $
			Lib.loadAttributesConfig config_attributesConfig
		return $ Lib.Config{
			Lib.config_port = config_port,
			Lib.config_pluginsConfig = config_pluginsConfig,
			Lib.config_attributesConfig = attributesCfg
		}

parserInfo :: ParserInfo Config
parserInfo =
	info (helper <*> parseConfig) $
		fullDesc

parseConfig :: Parser Config
parseConfig =
	Config <$>
		( option auto $ value 8080 <>
			long "port" <> short 'p' <> metavar "PORT"
				<> help "listen on PORT"
		)
		<*> parsePluginsCfg
		<*> (option str $ value "css_config.yaml" <> long "css-config" <> short 'c' <> help "the css config file")

parsePluginsCfg :: Parser Lib.PluginsConfig
parsePluginsCfg =
	M.fromList <$>
	many (option readParam $ long "plugin")
	where
		readParam :: ReadM (String, Lib.PluginConfig)
		readParam = str >>= \x ->
			case splitAtColon2 x of
				(plugin,mUri, mConfigFile) ->
					do
						when (not $ plugin `elem` Lib.pluginNames) $ readerError $ "unknown plugin: " ++ x
						case (mUri, mConfigFile) of
							(Just uri, Just configFile) ->
								return $ (plugin,) $ Lib.PluginConfig {
									Lib.plugin_uri = Lib.toURI uri,
									Lib.plugin_configFile = configFile
								}
							_ -> readerError "error parsin plugin parameters"

splitAtColon2 :: String -> (String, Maybe String, Maybe String)
splitAtColon2 s =
	let
		(x,y') = splitAtColon s
	in
		maybe (x,y',Nothing) (\(y,z) -> (x,Just y,z)) $
			splitAtColon <$> y'

splitAtColon :: String -> (String, Maybe String)
splitAtColon s =
	case span (/=':') s of
		(x,':':y) -> (x, Just y)
		(x,[]) -> (x, Nothing)
		_ -> error "splitAtColon error" -- <- should never occur
