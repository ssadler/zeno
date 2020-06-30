
module TestUtils
  ( module Out
  , TestIO(..)
  , (@?=)
  , testIOCase
  ) where


import           Control.Monad.Logger
import           Test.Tasty.HUnit as Out hiding ((@?=))
import           Test.Hspec as Out
import           Test.QuickCheck as Out hiding (Fixed)
import           Test.Tasty.QuickCheck as Out hiding (Fixed)
import           Test.Tasty as Out hiding (after, after_)
import qualified Test.Tasty.HUnit as HUnit
import           GHC.Stack as Out (HasCallStack)
import           Debug.Trace as Out

import Control.Monad.IO.Class

(@?=) :: (HasCallStack, MonadIO m, Eq a, Show a) => a -> a -> m ()
(@?=) a b = liftIO $ a HUnit.@?= b


newtype TestIO a = TestIO { runTestIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)
instance MonadLogger TestIO where monadLoggerLog a b c d = pure ()

testIOCase s act = testCase s $ runTestIO act
