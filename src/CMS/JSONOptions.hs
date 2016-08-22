module CMS.JSONOptions where


import Data.Aeson.TH
import Data.Char

jsonOptions = defaultOptions{
{-
	fieldLabelModifier =
		stripPrefix,
-}
	constructorTagModifier =
		map toLower,
		--stripPrefix
	sumEncoding = ObjectWithSingleField
	--unwrapUnaryRecords = True
}

stripPrefix x =
	if ('_' `elem` x)
	then
		(drop 1 . dropWhile (/='_')) x
	else x
