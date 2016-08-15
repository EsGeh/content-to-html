{-# LANGUAGE OverloadedStrings #-}
module Lib(
	Config(..),
	runHomepage
) where

import Data.Monoid
import Web.Spock.Safe


data Config
	= Config {
		config_port :: Int
	}
	deriving (Show, Read)

runHomepage :: Config -> IO ()
runHomepage conf =
	let
		port = config_port conf
	in
		runSpock port $ spockT id $
		do
			get root $
				text "Hello World!"
			get ("hello" <//> var) $ \name ->
				text ("Hello " <> name <> "!")
