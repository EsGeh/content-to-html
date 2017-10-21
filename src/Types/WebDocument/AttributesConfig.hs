{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.WebDocument.AttributesConfig where

import Types.WebDocument
import Utils.Yaml
import Utils.JSONOptions

import qualified Lucid.Base as Lucid
import GHC.Generics
import Data.Aeson.TH
import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Data.Map as M


data AttributesCfg
	= AttributesCfg {
		attributes_menuSection :: Attributes,
		attributes_mainSection :: Attributes,
		attributes_menuClasses :: MenuAttributes,
		attributes_sectionHeading :: [Attributes],
		attributes_section :: [Attributes],
		attributes_formAttributes :: FormAttributes
	}
	deriving( Eq, Ord, Show, Read, Generic )

data FormAttributes
	= FormAttributes {
		formAttributes_form :: Attributes,
		formAttributes_inputFieldDiv :: Attributes,
		formAttributes_label :: Attributes,
		formAttributes_textArea :: Attributes,
		formAttributes_input :: Attributes
	}
	deriving( Eq, Ord, Show, Read, Generic )

data MenuAttributes
	= MenuAttributes {
		menuAttributes_menu :: [Attributes], -- ^ <ul class=... > ... </ul>
		menuAttributes_entryClasses :: [Attributes], -- ^ <li class=...> ... </li>
		menuAttributes_link :: [Attributes],
		menuAttributes_categoryClasses :: [Attributes], -- ^ <li class=...> <ul> ... </ul> </li>
		menuAttributes_categoryLink :: [Attributes]
	}
	deriving( Eq, Ord, Show, Read, Generic )

--type AttributesClass = String
-- type Attributes = [(T.Text, T.Text)]

loadAttributesConfig ::
	(MonadIO m, MonadError String m) =>
	FilePath -> m AttributesCfg
loadAttributesConfig = loadYaml

attributesToLucid :: Attributes -> [Lucid.Attribute]
attributesToLucid =
	map (uncurry Lucid.makeAttribute) . M.toList

{-
instance FromJSON Attributes where
instance FromJSON MenuAttributes where
instance FromJSON MenuEntryAttributes where
-}

$(deriveJSON jsonOptions ''AttributesCfg)
$(deriveJSON jsonOptions ''MenuAttributes)
$(deriveJSON jsonOptions ''FormAttributes)
