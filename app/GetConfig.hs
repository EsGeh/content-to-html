{-# LANGUAGE TupleSections #-}
module GetConfig(
	getConfig
) where

import Lib

import Options.Applicative
import qualified Data.Map as M
import Control.Monad
--import qualified Data.Text as T


getConfig :: IO Config
getConfig =
	execParser parserInfo


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
		-- <*> parseSharedDirsConfig
		<*> parsePluginsCfg
		-- <*> parseProjDBConfig
		{-
		<*> ( option readUserCss $ value Nothing <>
			long "user_css" <> metavar "USER_CSS"
				<> help "user defined css to be included"
		)
		<*> (option str $
			long "content" <> metavar "CONTENT_TREE"
			<> help "file defining the website hierarchy"
		)
	where
		readUserCss = Just <$> str
	-}

parsePluginsCfg :: Parser PluginsConfig
parsePluginsCfg =
	M.fromList <$>
	many (option readParam $ long "plugin")
	where
		readParam :: ReadM (String, PluginConfig)
		readParam = str >>= \x ->
			case splitAtColon2 x of
				(plugin,mUri, mConfigFile) ->
					do
						when (not $ plugin `elem` pluginNames) $ readerError $ "unknown plugin: " ++ x
						case (mUri, mConfigFile) of
							(Just uri, Just configFile) ->
								return $ (plugin,) $ PluginConfig {
									plugin_uri = toURI uri,
									plugin_configFile = configFile
								}
							_ -> readerError "error parsin plugin parameters"

{-
	many $
		PluginConfig <$>
			(option str $
-}

{-
parseProjDBConfig :: Parser ProjDBConfig
parseProjDBConfig =
	mlConfig <$>
		( option str $ value "demo/projDB.yaml" <>
			long "projDB" <> metavar "PROJ_DB_FILE"
				<> help "file containing projects data"
		)
-}

{-
parseSharedDirsConfig :: Parser [DirConfig]
parseSharedDirsConfig =
		many $ option readOptionParam $
			long "data" <> metavar "DATA_DIR[:URL_PREFIX]"
				<> help "directory with data to be shared optional followed by a colon and a url prefix"
	where
		readOptionParam :: ReadM DirConfig
		readOptionParam = fmap `flip` str $ \x ->
			case splitAtColon x of
				(path, Nothing) -> defDirConfig path
				(path, Just uriPrefix) ->
					DirConfig {
						dirConfig_path = path,
						dirConfig_uriPrefix = uriPrefix
					}
-}

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
