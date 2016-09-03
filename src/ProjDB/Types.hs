{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module ProjDB.Types where

import CMS.JSONOptions

import Data.Aeson.TH
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Map as M
import Lens.Micro.Platform
import qualified Language.Haskell.TH.Syntax as TH

data ProjDB =
	ProjDB {
		db_artists :: M.Map ArtistKey Artist,
		db_projects :: M.Map ProjectKey Project,
		db_persons :: M.Map PersonKey Person
	}

type ArtistKey = Name
type ProjectKey = Name
type PersonKey = Name

type Name = T.Text

allArtists, allProjects, allPersons :: ProjDB -> [Name]
allArtists = M.keys . db_artists
allProjects = M.keys . db_projects
allPersons = M.keys . db_persons

lookupArtist :: ArtistKey -> ProjDB -> Maybe Artist
lookupArtist key = M.lookup key . db_artists
lookupProject :: ProjectKey -> ProjDB -> Maybe Project
lookupProject key = M.lookup key . db_projects
lookupPerson :: PersonKey -> ProjDB -> Maybe Person
lookupPerson key = M.lookup key . db_persons

projectsFromArtist :: Name -> ProjDB -> [Project]
projectsFromArtist key db =
	-- join $
	M.elems $
	M.filter ((key `elem`) . project_artist) $
	db_projects db

data Entry
	= ArtistEntry Artist
	| ProjectEntry Project
	| PersonEntry Person
	deriving( Read, Show, Generic, Eq, Ord )

data Artist
	= Artist {
		artist_name :: Name, -- key
		artist_persons :: [PersonKey]
	}
	deriving( Read, Show, Generic, Eq, Ord )

data Project
	= Project {
		project_name :: Name, -- key
		project_artist :: [ArtistKey]
	}
	deriving( Read, Show, Generic, Eq, Ord )

data Person
	= Person {
		person_name :: T.Text,
		person_born :: Maybe Date,
		person_dead :: Maybe Date
	}
	deriving( Read, Show, Generic, Eq, Ord )

data Date
	= Date
	deriving( Read, Show, Generic, Eq, Ord )


flip makeLensesWith ''ProjDB $
	lensRules
		& lensField .~ (\_ _ field -> [ TopName $ TH.mkName $ TH.nameBase field ++ "_L"])

$(deriveJSON jsonOptions ''Entry)
$(deriveJSON jsonOptions ''Artist)
$(deriveJSON jsonOptions ''Project)
$(deriveJSON jsonOptions ''Person)
$(deriveJSON jsonOptions ''Date)

projDBFromEntries :: [Entry] -> ProjDB
projDBFromEntries =
	($ projDBDef)
	.
	foldl (.) id
	.
	(map $
	\entry ->
		case entry of
			ArtistEntry x -> insertArtist x
			PersonEntry x -> insertPerson x
			ProjectEntry x -> insertProject x
	)

projDBToEntries :: ProjDB -> [Entry]
projDBToEntries db =
	(fmap ArtistEntry $ M.elems $ db_artists db)
	++
	(fmap ProjectEntry $ M.elems $ db_projects db)
	++
	(fmap PersonEntry $ M.elems $ db_persons db)

projDBDef = ProjDB M.empty M.empty M.empty

insertArtist :: Artist -> ProjDB -> ProjDB
insertArtist x =
	over db_artists_L $
		M.insert (artist_name x) x
insertPerson x =
	over db_persons_L $
		M.insert (person_name x) x
insertProject x =
	over db_projects_L $
		M.insert (project_name x) x

{-

type MusicList = [Entry]

data Entry
	= Entry {
		entry_artist :: T.Text,
		entry_title :: T.Text,
		entry_comment :: T.Text
	}
	deriving( Read, Show, Generic, Eq, Ord )

type Key = (T.Text, T.Text) -- Artist, Title

{-
instance FromJSON Entry
instance ToJSON Entry
-}

$(deriveJSON jsonOptions ''Entry)
-}
