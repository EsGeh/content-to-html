module Main where

import GetConfig( getConfig )
import Lib( runHomepage )


main :: IO ()
main =
	runHomepage =<< getConfig
