{-# LANGUAGE OverloadedStrings #-}
module Html where

import Lucid
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Monoid


type Title = T.Text

renderPage :: Html () -> T.Text
renderPage =
	LT.toStrict . renderText

basePage :: Title -> Html () -> Html ()
basePage title content =
	html_ $ do
		head_ $ do
			meta_ [charset_ "UTF-8"]
			link_ [rel_ "stylesheet", href_ "http://www.w3schools.com/lib/w3.css"]
			link_ [rel_ "stylesheet", href_ "/css/style.css"]
			title_ $ toHtml title
		body_ $
			content

calcNavLinks :: FilePath -> [(FilePath, T.Text)] -> Zipper (FilePath, T.Text)
calcNavLinks activeRoute =
	(\(before, active:after) ->  (before, active, after)) .
	break (\x -> fst x == activeRoute)

type Zipper a = ([a],a,[a])

nav :: Zipper (FilePath, T.Text) -> Html ()
nav (before, active, after) =
	nav_ $ ul_ [class_ "w3-navbar w3-border w3-light-blue"] $
		mapM_ (li_ . link) before
		<>
		(li_ [class_ "w3-blue"] . link) active
		<>
		mapM_ (li_ . link) after
	where
		link :: (FilePath, T.Text) -> Html ()
		link (route, name) =
			a_ [href_ $ T.pack route] (toHtml name)
