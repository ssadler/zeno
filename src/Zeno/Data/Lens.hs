
module Zeno.Data.Lens where
 
import Control.Monad.State (MonadState, get)
import Data.Char (isUpper, toLower)
import Data.List (nub, findIndices, stripPrefix, isPrefixOf)
import Data.Maybe
import Data.Monoid
import Lens.Micro.Platform hiding ((.=), makeLenses)
import Language.Haskell.TH


makeLensesUnderscored = makeLensesWith $
  set lensField underscoredNamer abbreviatedFields

-- | Given a field someField creates a lens _field
--
underscoredNamer :: Name -> [Name] -> Name -> [DefName]
underscoredNamer _ fields field = maybeToList $ do

  fieldPart <- stripMaxLc (nameBase field)
  method    <- computeMethod fieldPart
  let cls = "Has" ++ fieldPart
  return (MethodName (mkName cls) (mkName method))

  where
  stripMaxLc f = do x <- stripPrefix optUnderscore f
                    case break isUpper x of
                      (p,s) | null p || null s -> Nothing
                            | otherwise        -> Just s
  optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]

  computeMethod (x:xs) | isUpper x = Just ('_' : toLower x : xs)
  computeMethod _                  = Nothing


useList :: MonadState s m => Getting (Endo [a]) s a -> m [a]
useList l = toListOf l <$> get


