module Types.Resource(
	module Types.Resource,
) where

import Types.WebDocument
import Types.URI

import qualified Data.Text as T
import Control.Monad.Identity


-- |a resource that the server provides.
type Resource = ResourceGen Section

-- |a resource can be a full page including a navigation
data ResourceGen section
	= FullPageResource PageWithNav
	| PageResource section
	| FileResource FileResInfo
	deriving( Show, Read )

resource_mapToPageResourceM ::
	Monad m =>
	(a -> m b) -> ResourceGen a -> m (ResourceGen b)
resource_mapToPageResourceM f (PageResource x) =
	PageResource <$> f x
resource_mapToPageResourceM _ (FullPageResource x) = return $ FullPageResource x
resource_mapToPageResourceM _ (FileResource x) = return $ FileResource x

resource_mapToPageResource ::
	(a -> b) -> ResourceGen a -> ResourceGen b
resource_mapToPageResource f =
	runIdentity . resource_mapToPageResourceM (return . f)

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

-- $(Lens.makeLensesWith lensRules' ''ResourceGen)
