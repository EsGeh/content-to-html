{-# LANGUAGE OverloadedStrings #-}
module Html where

import Lucid

import qualified Data.Text as T
import Data.Monoid


type Title = String

basePage :: Title -> Html () -> Html ()
basePage title content =
	html_ $ do
		head_ $ do
			meta_ [charset_ "UTF-8"]
			link_ [rel_ "stylesheet", href_ "http://www.w3schools.com/lib/w3.css"]
			title_ $ toHtml title
		body_ $
			content

calcNavLinks :: T.Text -> [(T.Text, T.Text)] -> Zipper (T.Text, T.Text)
calcNavLinks activeRoute =
	(\(before, active:after) ->  (before, active, after)) .
	break (\x -> snd x == activeRoute)

type Zipper a = ([a],a,[a])

nav :: Zipper (T.Text, T.Text) -> Html ()
nav (before, active, after) =
	nav_ $ ul_ [class_ "w3-navbar w3-border w3-light-blue"] $
		mapM_ (li_ . link) before
		<>
		(li_ [class_ "w3-blue"] . link) active
		<>
		mapM_ (li_ . link) after
	where
		link :: (T.Text, T.Text) -> Html ()
		link (name, route) =
			a_ [href_ route] (toHtml name)

info :: Html ()
info =
	(textContent "info") $
	mconcat $
	[ section "first section" "bla"
	, section "second section" "bli <emph> bla </emph>"
	]

textContent :: Title -> Html () -> Html ()
textContent title sections =
	article_ [class_ "w3-container"] $ do
		header_ [class_ "w3-container w3-light-blue"] $ h1_ $ toHtml title
		sections

section :: Title -> Html () -> Html ()
section title content =
	section_ [class_ "w3-panel w3-border"] $ do
		header_ [class_ "w3-light-blue"] $ h2_ $ toHtml title
		p_ $ content
