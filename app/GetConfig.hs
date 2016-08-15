module GetConfig(
	getConfig
) where

import Options.Applicative
import Lib


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
