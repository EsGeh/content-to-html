{-# LANGUAGE DeriveGeneric #-}
module Types.URI(
	URI(),
	toURI, fromURI,
	uriFromList, uriToList,
	uriSplitPrefix,
) where

import Data.Aeson
import System.FilePath.Posix
import GHC.Generics
import Data.List


newtype URI = URI { fromURI :: FilePath }
	deriving( Eq, Ord, Show, Read, Generic )

toURI :: FilePath -> URI
toURI =
	URI . normalizeURI
	where
		normalizeURI =
			("/" </>) -- uris have to start with "/"!
			. normalise -- try to transform to a normal form

uriFromList :: [FilePath] -> URI
uriFromList uri =
	toURI $ intercalate "/" uri

uriToList :: URI -> [FilePath]
uriToList =
	map dropTrailingPathSeparator .
	splitPath .
	dropDrive .
	fromURI

uriSplitPrefix :: URI -> (URI, URI)
uriSplitPrefix uri =
{-
	case uriToList uri of
		(x:xs) -> Just $ (toURI $ x, toURI $ joinPath xs)
		_ -> Nothing
-}
	case uriToList uri of
		(x:xs) -> (toURI $ x, toURI $ joinPath xs)
		[] -> (toURI "", toURI "")

instance FromJSON URI where
	parseJSON = (toURI <$>) . parseJSON

instance ToJSON URI where
	toJSON = toJSON . fromURI
