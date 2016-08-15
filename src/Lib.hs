{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runHomepage
    ) where

import Data.Monoid
import Web.Spock.Safe


runHomepage :: IO ()
runHomepage =
	runSpock 8080 $ spockT id $
	do
		get root $
			text "Hello World!"
		get ("hello" <//> var) $ \name ->
			text ("Hello " <> name <> "!")
