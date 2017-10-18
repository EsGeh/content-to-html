{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Plugins.HierarchicWebsite where

import Types.Resource
import Types.WebDocument
import Plugins.HierarchicWebsite.Types
import Plugins
import Types.URI
import Utils.Yaml
import Utils.JSONOptions
import Data.Aeson.TH
import qualified Data.Yaml as Yaml( Value(..) )

import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import qualified System.Directory as Dir
import System.IO.Error
import System.FilePath.Posix
import Data.List
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T


data RoutesState = 
	RoutesState {
		content :: Content,
		routes :: Routes EmbeddablePlaceholder,
		userCss :: [URI],
		addToHeader :: T.Text
	}
	deriving( Show, Read )

data Config
	= Config {
		config_sharedDirs :: [DirConfig],
		config_userCSS :: [URI],
		config_addToHeader :: T.Text,
		config_content :: FilePath,
		config_defaultRoute :: Maybe URI
	}
	deriving (Generic, Show, Read)

type Routes placeHolder = M.Map URI (ResourceTemplate placeHolder)
type PageRoutes placeHolder = M.Map URI (SectionTemplate placeHolder)
type FileRoutes = M.Map URI FileResInfo

-- |a resource which can include further requests to generate sections.
type ResourceTemplate placeHolder = ResourceGen (SectionTemplate placeHolder)

data EmbeddableConfig
	= EmbeddableConfig {
		embeddableCfg_instance :: String,
		embeddableCfg_embeddable :: String,
		--embeddableCfg_placeholder :: EmbeddablePlaceholder,
		embeddableCfg_params :: Yaml.Value
	}
	deriving( Show, Read, Eq, Generic)

data EmbeddablePlaceholder
	= EmbeddablePlaceholder{
		placeholder_instance :: String,
		placeholder_embeddable :: String
	}
	deriving( Show, Read, Ord, Eq, Generic)

placeHolderFromConfig :: EmbeddableConfig -> EmbeddablePlaceholder
placeHolderFromConfig EmbeddableConfig{..} =
	EmbeddablePlaceholder{
		placeholder_instance = embeddableCfg_instance,
		placeholder_embeddable = embeddableCfg_embeddable
	}

plugin :: Plugins.MainPlugin RoutesState
plugin = Plugins.MainPlugin {
	mainPlugin_answerReq = fmap Just . answer_req,
	mainPlugin_descr = "hierarchic website"
}

load :: Plugins.MainLoader RoutesState
load configFile =
	(either (throwError . show) return =<< liftIO (decodeFileEither configFile)) >>= \Config{..} ->
	do
		sharedData <-
			loadSharedData config_defaultRoute config_sharedDirs
				`catchError` \e ->
				throwError ("error while loading sharedData: " ++ e)
					-- :: m (M.Map URI (ResourceTemplate EmbeddableConfig))
		contentTree <-
			loadContent config_content
		let
			initState :: RoutesState
			initState =
				RoutesState {
					content = contentTree,
					routes =
						fmap `flip` sharedData $
							resource_mapToPageResource $ mapToPlaceHolders placeHolderFromConfig,
					userCss = config_userCSS,
					addToHeader = config_addToHeader
				}
			embeddables :: M.Map EmbeddableInstanceID (EmbeddableName, Value)
			embeddables =
				M.unions $
				map `flip` (M.elems sharedData) $ collectEmbeddables
				-- _ :: M.Map EmbeddableInstanceID (EmbeddableName, Yaml.Value)
		return
			(plugin, initState, embeddables)

answer_req ::
	(MonadIO m, MonadError String m) =>
	Request -> Plugins.RunReqT RoutesState m Resource
answer_req (resKey,_) =
	get >>= \RoutesState{..} ->
		findPage resKey routes >>= \case
			PageResource x ->
				fmap (FullPageResource . (PageWithNav (calcNav content) `flip` (HeaderInfo userCss addToHeader))) $
				fillTemplate x
			FullPageResource res -> return $ FullPageResource $ res
			FileResource res -> return $ FileResource $ res

fillTemplate ::
	(MonadIO m, MonadError String m) =>
	SectionTemplate EmbeddablePlaceholder ->
	Plugins.RunReqT RoutesState m Section
fillTemplate = \case
	SectionEntry (Left EmbeddablePlaceholder{..}) ->
		lift $ Plugins.execEmbeddable placeholder_instance
	SectionEntry (Right info) -> return $ SectionEntry info
	SectionNode info ->
		fmap SectionNode $
		sectionInfo_mapToContentM `flip` info $ mapM fillTemplate

collectEmbeddables :: 
	(ResourceTemplate EmbeddableConfig)
	-> M.Map EmbeddableInstanceID (EmbeddableName, Value)
collectEmbeddables =
	M.fromList .
		\case
			PageResource section' -> collectEmbeddables' section'
			_ -> []
	where
		collectEmbeddables' ::
			SectionGen (Either EmbeddableConfig SectionInfo)
			-> [(EmbeddableInstanceID, (EmbeddableName, Value))]
		collectEmbeddables' =
				eitherSection l r
		l ::
			Either EmbeddableConfig SectionInfo
			-> [(EmbeddableInstanceID, (EmbeddableName, Value))]
		l (Left EmbeddableConfig{..}) =
			[
				( embeddableCfg_instance
				, (embeddableCfg_embeddable, embeddableCfg_params)
				)
			]
		l _ = []
		r ::
			SectionNodeInfo (Either EmbeddableConfig SectionInfo)
			-> [(EmbeddableInstanceID, (EmbeddableName, Value))]
		r = join . map collectEmbeddables' . section_content

mapToPlaceHolders ::
	forall a b .
	(a -> b)
	-> SectionTemplate a -> SectionTemplate b
mapToPlaceHolders f =
	eitherSection l r
	where
		l :: Either a SectionInfo -> SectionGen (Either b SectionInfo)
		l = SectionEntry . either (Left . f) Right
		r :: SectionNodeInfo (Either a SectionInfo) -> SectionGen (Either b SectionInfo)
		r = SectionNode . sectionInfo_mapToContent (map $ mapToPlaceHolders f)


calcNav :: Content -> [NavEntry]
calcNav content =
	map `flip` content $ \e ->
		case content_subEntries e of
			Left uri -> NavEntry $ Link (content_caption e) uri
			Right subEntries ->
				NavCategory (content_caption e) $
				calcNav subEntries

data DirConfig
	= DirConfig {
		dirConfig_path :: FilePath,
		dirConfig_uriPrefix :: FilePath
	}
	deriving( Show, Read, Generic )

defDirConfig :: FilePath -> DirConfig
defDirConfig path = DirConfig path path

loadSharedData ::
	forall m .
	(MonadIO m, MonadError String m) =>
	Maybe URI -> [DirConfig] -> m (M.Map URI (ResourceTemplate EmbeddableConfig))
loadSharedData mDefaultRoute sharedDirs =
	--((\x -> do{ liftIO $ print x; return x} ) =<<) $
	(maybe return addDefRoute mDefaultRoute =<<) $
	fmap combineRoutes $
	forM sharedDirs $ \DirConfig{..} ->
		do
			liftIO $ putStrLn $ "-----------------------------------------"
			liftIO $ putStrLn $ concat [ "loading \"", dirConfig_path, "\"..." ]
			loadFilesInDir `flip` dirConfig_path $
				defLoadDirInfo dirConfig_uriPrefix dirConfig_path
	where
		addDefRoute :: URI -> (M.Map URI (ResourceTemplate EmbeddableConfig)) -> m (M.Map URI (ResourceTemplate EmbeddableConfig))
		addDefRoute defRoute routes =
			do
				liftIO $ putStrLn $ "default route: " ++ fromURI defRoute
				addRoute (toURI "") `flip` routes <$> (findPage defRoute routes)

-----------------------------------
-- create a Routes object:
-----------------------------------

combineRoutes ::
	[Routes placeHolder] -> Routes placeHolder
combineRoutes =
	M.unions

addRoute :: URI -> (ResourceTemplate placeHolder) -> Routes placeHolder -> Routes placeHolder
addRoute =
	M.insert

loadFilesInDir ::
	forall m .
	(MonadIO m, MonadError String m) =>
	(FilePath -> m (Maybe (URI, (ResourceTemplate EmbeddableConfig))))
	-> FilePath -> m (M.Map URI (ResourceTemplate EmbeddableConfig))
loadFilesInDir calcRes dirPath =
	calc =<< (ls dirPath :: m [FilePath])
	where
		calc :: [FilePath] -> m (M.Map URI (ResourceTemplate EmbeddableConfig))
		calc paths = 
			fmap (M.fromList . catMaybes) $
			mapM `flip` paths $ \path ->
				do
					mRes <- calcRes path
					case mRes of
						Nothing -> return ()
						Just (uri, _) ->
							liftIO $ putStrLn $ concat $
							[ fromURI uri, " -> ", dirPath </> path ]
					return mRes 

prettyTemplate :: ResourceGen a -> String
prettyTemplate = \case
	FullPageResource _ -> "FullPageResource"
	PageResource _ -> "PageResource"
	FileResource FileResInfo{..} -> concat [ "File \"", fileRes_file, "\"" ]

-----------------------------------
-- filter/search routes:
-----------------------------------

routes_pages :: Routes placeHolder-> PageRoutes placeHolder
routes_pages =
	M.mapMaybe (\r -> case r of { PageResource page -> Just page; _ -> Nothing}) 

routes_files :: Routes placeHolder -> FileRoutes
routes_files =
	M.mapMaybe (\r -> case r of { FileResource info -> Just info; _ -> Nothing}) 

findPage ::
	(MonadError String m) => URI -> Routes placeHolder -> m (ResourceTemplate placeHolder)
findPage key routes =
	let mRes = M.lookup key routes in
		case mRes of
			Just res -> return res
			_ ->
				throwError $
				concat $
				[ "could not find \""
				, fromURI key
				, "\"!"
				, "\npossible: ", show $ M.keys routes
				]

defLoadDirInfo ::
	(MonadIO m, MonadError String m) =>
	FilePath -> FilePath -> FilePath -> m (Maybe (URI, (ResourceTemplate EmbeddableConfig)))
defLoadDirInfo uriPrefix dir path =
	case takeExtension path of
		".mp3" -> return $ Just $
			( toURI $ uriPrefix </> path
			, FileResource $ defResource { fileRes_type = ResType $ "audio/mpeg" }
			)
		".pdf" -> return $ Just $
			( toURI $ uriPrefix </> path
			, FileResource $ defResource { fileRes_type = ResType $ "application/pdf" }
			)
		".css" -> return $ Just $
			( toURI $ uriPrefix </> path
			, FileResource $ defResource { fileRes_type = ResType $ "style/css" }
			)
		".yaml" ->
			Just <$>
			( toURI $ uriPrefix </> dropExtension path, ) <$>
			PageResource <$>
			loadYaml (dir </> path)
		_ -> return $ Just $
			( toURI $ uriPrefix </> path
			, FileResource $ defResource
			)
	where
		defResource =
			FileResInfo {
				fileRes_type = ResType $ "unknown",
				fileRes_file = dir </> path
			}

instance FromJSON (SectionTemplate EmbeddableConfig) where
	parseJSON = withObject "section" $ \x ->
		-- this is a leaf in the tree of sections
		((SectionEntry . fromPlaceholderOrSection) <$> parseJSON (Object x))
		<|>
		( -- this is an inner node in the tree of sections
			do
				title <- x .:? "title"
				content <- x .: "subsections"
				props <-
					fromMaybe attributes_empty <$> x .:? "attributes"
				return $
					SectionNode $
					(defSectionInfo content){
						section_title = title,
						section_attributes = props
					}
		)

newtype PlaceholderOrSection = PlaceholderOrSection { fromPlaceholderOrSection :: Either EmbeddableConfig SectionInfo }

instance FromJSON PlaceholderOrSection where
	parseJSON = withObject "section" $ \o ->
		((PlaceholderOrSection . Right) <$> parseJSON (Object o))
		<|>
		(
			(PlaceholderOrSection . Left) <$> parseJSON (Object o)
			{-
			do
				uri <- o .: "uri"
				mParams <-
					o.:? "params"
				return (uri, M.toList $ fromMaybe M.empty mParams)
				-}
		)

-----------------------------------
-- utils:
-----------------------------------

ls ::
	(MonadIO m, MonadError String m) =>
	FilePath -> m [FilePath]
ls dir =
	-- map (dir </>) <$> 
	filter (not . ("." `isPrefixOf`)) <$>
	do
		mRet <- liftIO (tryIOError $ Dir.getDirectoryContents dir)
		either
			(throwError . show)
			return
			mRet

$(deriveJSON jsonOptions ''EmbeddablePlaceholder)
$(deriveJSON jsonOptions ''EmbeddableConfig)
instance FromJSON Config where
instance ToJSON Config where
instance FromJSON DirConfig where
instance ToJSON DirConfig where
