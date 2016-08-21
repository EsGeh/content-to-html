{-# LANGUAGE OverloadedStrings #-}
module RenderPage where

import Lucid

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT


renderPage :: Html () -> T.Text
renderPage =
	LT.toStrict . renderText
