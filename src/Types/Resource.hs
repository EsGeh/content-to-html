module Types.Resource(
	module Types.Resource,
) where

import Types.WebDocument
import Types.URI

import qualified Data.Text as T


-- |a resource that the server provides.
type Resource = ResourceGen Section

-- |a resource which can include further requests to generate sections.
type ResourceTemplate = ResourceGen (SectionTemplate Request)

-- |a resource can be a full page including a navigation
data ResourceGen section
	= FullPageResource PageWithNav
	| PageResource section
	| FileResource FileResInfo
	deriving( Show, Read )

data FileResInfo
	= FileResInfo {
		fileRes_type :: ResType,
		fileRes_file :: FilePath
	}
	deriving( Show, Read )

type Request = (URI, Params)
type Params = [(T.Text, T.Text)]
-- type Params = M.Map T.Text T.Text

newtype ResType = ResType { fromResType :: T.Text }
	deriving( Eq, Ord, Show, Read )
