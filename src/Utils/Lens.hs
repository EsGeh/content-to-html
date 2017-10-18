module Utils.Lens where

import qualified Lens.Micro.Platform as Lens
import Language.Haskell.TH.Syntax


lensRules' :: Lens.LensRules
lensRules' = (Lens.set Lens.lensField `flip` Lens.lensRules) $ lensField

lensField :: Name -> [Name] -> Name -> [Lens.DefName]
lensField _ _ n =
	[Lens.TopName (mkName $ nameBase n ++ "_L")]
{-
  case nameBase n of
    '_':x:xs -> [Lens.TopName (mkName (toLower x : xs))]
    _        -> [] 
-}

