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
		config_pluginsConfig :: Lib.MainPluginsConfig,
		config_embeddablesConfig :: Lib.EmbeddablesConfig,
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
			Lib.config_pluginsConfig = Lib.PluginsConfig {
				Lib.config_mainPluginsConfig = config_pluginsConfig,
				Lib.config_embeddablesConfig = config_embeddablesConfig
			},
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
		<*> parseEmbeddablesCfg
		<*> (option str $ value "css_config.yaml" <> long "css-config" <> short 'c' <> help "the css config file")

parseEmbeddablesCfg :: Parser Lib.EmbeddablesConfig
parseEmbeddablesCfg =
	M.fromList <$>
	many (option readParam $ long "embeddable")
	where
		readParam :: ReadM (String, FilePath)
		readParam = str >>= \x ->
			do
				let (name, mConfigFile) = splitAtColon x
				when (not $ name `elem` Lib.embeddableNames) $ readerError $ "unknown embeddable: " ++ x
				configFile <- maybe (readerError $ "error parsing embeddable parameters") return $ mConfigFile
				return $ (name, configFile)

parsePluginsCfg :: Parser Lib.MainPluginsConfig
parsePluginsCfg =
	M.fromList <$>
	many (option readParam $ long "plugin")
	where
		readParam :: ReadM (String, Lib.MainPluginConfig)
		readParam = str >>= \x ->
			case splitAtColon2 x of
				(plugin,mUri, mConfigFile) ->
					do
						when (not $ plugin `elem` Lib.mainPluginNames) $ readerError $ "unknown plugin: " ++ x
						case (mUri, mConfigFile) of
							(Just uri, Just configFile) ->
								return $ (plugin,) $ Lib.MainPluginConfig {
									Lib.mainPlugin_uri = Lib.toURI uri,
									Lib.mainPlugin_configFile = configFile
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
