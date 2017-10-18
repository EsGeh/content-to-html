{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Plugins.Form where

import Types.Resource
import Types.WebDocument
import Types.URI
import Plugins
import Utils.Yaml
import Utils.JSONOptions
import Data.Aeson.TH

import qualified Network.HaskellNet.SMTP.SSL as Email
import qualified Network.Mail.Mime as Email
-- import qualified Network.Mail.SMTP as Email
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.Lazy as L


plugin :: Plugins.Embeddable () FormState
plugin = Plugins.defaultEmbeddable {
	Plugins.embeddable_answerInternalReq = embedd,
	Plugins.embeddable_answerReq = \_ -> handleFormData,
	Plugins.embeddable_descr = "html forms to deal with user input"
}

load ::
	(MonadIO m, MonadError String m) =>
	FilePath -> Plugins.EmbeddableLoader m
load configFile _ =
	loadYaml configFile >>= \(config :: Config) ->
	--(either (throwError . show) return =<< liftIO (decodeFileEither configFile)) >>= \Config{..} ->
	do
		liftIO $ putStrLn "loading website..."
		return $ EmbeddableStateCont $
			(plugin, (), FormState config)
		{-
		return $
			(plugin,) $
			FormState $
			config
		-}

data FormState
	= FormState {
		formState_config :: Config
	}
	deriving (Generic, Show, Read)

data Config
	= Config {
		config_onSubmit :: OnSubmitAction
	}
	deriving (Generic, Show, Read)

data OnSubmitAction
	= SendEmail EmailInfo
	| DoNothing
	deriving (Generic, Show, Read)

data EmailInfo
	= EmailInfo {
		email_hostname :: String,
		email_port :: Int,
		email_auth :: Maybe AuthInfo,
		email_from :: Address,
		email_to :: [Address],
		email_cc :: [Address],
		email_bcc :: [Address],
		email_subject :: T.Text
	}
	deriving (Generic, Show, Read)

newtype Address = Address { fromAddress :: Email.Address }

instance FromJSON Address where
	parseJSON = (Address <$>) . parseJSON

instance ToJSON Address where
	toJSON = toJSON . fromAddress

instance Read Address where
	readsPrec = error "not yet defined"

instance Show Address where
	show = show . fromAddress

data AuthInfo
	= AuthInfo {
		auth_username :: String,
		auth_password :: String
	}
	deriving (Generic, Show, Read)

embedd ::
	(MonadIO m, MonadError String m) =>
	Plugins.EmbeddableInstanceID -> () -> Plugins.RunReqT FormState m Section
embedd instanceId _ =
	get >>= \_ ->
		return $ SectionEntry $ SectionInfo{
			section_title = Just "testForm",
			section_content = Form $ formInfo instanceId,
			section_attributes = attributes_empty
		}

handleFormData ::
	(MonadIO m, MonadError String m) =>
	() -> Request -> RunReqT FormState m (Maybe Resource)
handleFormData _ (uri,params) =
	get >>= \cfg ->
	if uri == toURI ""
	then
		do
			_ <- case config_onSubmit $ formState_config cfg of
				SendEmail EmailInfo{ email_hostname, email_port, email_auth = mAuthInfo, ..} ->
					liftIO $
					Email.doSMTPSTARTTLSWithSettings
						email_hostname
						Email.defaultSettingsSMTPSTARTTLS{ Email.sslPort = fromIntegral email_port } $
					\conn ->
					do
						maybe (return ()) `flip` mAuthInfo $ \AuthInfo{..} ->
							Email.authenticate authType auth_username auth_password conn >>= \authSucceeded ->
							when (not authSucceeded) $
								putStrLn "authentication failed!"
							{-
							if authSucceeded
								then putStrLn "authentication succeeded"
								else putStrLn "authentication failed!"
							-}
						Email.sendMimeMail2 `flip` conn $
							Email.Mail {
								Email.mailFrom = (fromAddress email_from),
								Email.mailTo = (fromAddress <$> email_to),
								Email.mailCc = (fromAddress <$> email_cc),
								Email.mailBcc = (fromAddress <$> email_bcc),
								Email.mailHeaders =
									[("Subject", email_subject)],
								Email.mailParts =
									[ [ Email.plainPart $ L.fromStrict $ T.pack $
										renderEmail $ params
									] ]
							}
						liftIO $ putStrLn $
							unlines $
							[ concat [ "email sent to ", show email_to ,". content:" ]
							, "---------------------"
							, renderEmail params
							, "---------------------"
							]
				DoNothing -> return ()
			return $ Nothing
	else
		throwError $ concat ["data request not defined \"", fromURI uri, "\"!" ]
	where
		authType = Email.PLAIN
		renderEmail =
			(unlines .) $ map $ \(field,value) ->
				concat [ T.unpack field, ": ", T.unpack value ]

formInfo :: String -> FormInfo
formInfo prefix = FormInfo{
	form_content =
		[ FormEntry{
				formEntry_caption = "test:",
				formEntry_type = TextAreaInput,
				formEntry_name = "input",
				formEntry_defValue = "please enter your message here..."
			}
		, FormEntry{
				formEntry_caption = "",
				formEntry_type = SubmitInput,
				formEntry_name = "Submit",
				formEntry_defValue = "Submit"
			}
		],
	form_action = T.pack $ prefix,
	form_method = Get
}

$(deriveJSON jsonOptions ''Config)
$(deriveJSON jsonOptions ''OnSubmitAction)
$(deriveJSON jsonOptions ''EmailInfo)
$(deriveJSON jsonOptions ''AuthInfo)
$(deriveJSON jsonOptions ''Email.Address)
