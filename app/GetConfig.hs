module GetConfig(
	getConfig
) where

import Lib
import Options.Applicative


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
		<*> parseSharedDirsConfig
		<*> parseProjDBConfig
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

parseProjDBConfig :: Parser ProjDBConfig
parseProjDBConfig =
	mlConfig <$>
		( option str $ value "demo/projDB.yaml" <>
			long "projDB" <> metavar "PROJ_DB_FILE"
				<> help "file containing projects data"
		)

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

splitAtColon :: String -> (String, Maybe String)
splitAtColon s =
	case span (/=':') s of
		(x,':':y) -> (x, Just y)
		(x,[]) -> (x, Nothing)
		_ -> error "splitAtColon error" -- <- should never occur
