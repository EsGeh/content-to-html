module GetConfig(
	getConfig
) where

import Lib
import Options.Applicative


getConfig :: IO Config
getConfig =
	execParser parserInfo


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
		<*> parseContentConfig
		<*> parseMLConfig

parseMLConfig :: Parser MLConfig
parseMLConfig =
	mlConfig <$>
		( option str $ value "musicList.yaml" <>
			long "musicList" <> metavar "MUSIC_LIST_FILE"
				<> help "file containing the music list"
		)

parseContentConfig :: Parser ContentConfig
parseContentConfig =
	contentConfig <$>
		( option str $ value "content" <>
			long "content" <> metavar "WEB_PAGE_CONTENT"
				<> help "file containing content"
		)
		<*>
		( option str $ long "css" <>
			value "css" <>
			metavar "CSS_DIR"
				<> help "dir containing css"
		)
		<*>
		( option str $ long "data" <>
			value "data" <>
			metavar "DATA_DIR"
				<> help "dir containing media data"
		)
