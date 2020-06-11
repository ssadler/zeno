
module TestUtils
  ( module Out
  , (@?=)
  ) where


-- import           Test.Tasty as Out hiding (after)
import           Test.Tasty.HUnit as Out hiding ((@?=))
import           Test.Hspec as Out
import           Test.QuickCheck as Out
import           Test.Tasty as Out hiding (after, after_)
import qualified Test.Tasty.HUnit as HUnit
import           GHC.Stack as Out (HasCallStack)


import Control.Monad.IO.Class
(@?=) :: (HasCallStack, MonadIO m, Eq a, Show a) => a -> a -> m ()
(@?=) a b = liftIO $ a HUnit.@?= b
