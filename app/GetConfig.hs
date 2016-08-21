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
		<*> parseMLConfig

parseMLConfig :: Parser MLConfig
parseMLConfig =
	mlConfig <$>
		( option str $ value "musicList.yaml" <>
			long "musicList" <> metavar "MUSIC_LIST_FILE"
				<> help "file containing the music list"
		)
