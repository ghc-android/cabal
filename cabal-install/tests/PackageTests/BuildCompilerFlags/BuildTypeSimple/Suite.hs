module PackageTests.BuildCompilerFlags.BuildTypeSimple.Suite ( test ) where

import PackageTests.BuildCompilerFlags.Util
import qualified PackageTests.PackageTester as PT

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import System.FilePath ((</>))

dir :: FilePath
dir = "PackageTests" </> "BuildCompilerFlags" </> "BuildTypeSimple"

test :: FilePath -> Test
test cabalPath =
  testGroup "BuildTypeSimple"
  [
    testCase
    "Save command line setup compiler settings in 'LocalBuildInfo'" $
    withTestDir $ do
      res <- configure [testBuildCompOpt, testBuildHcPkgOpt]
      PT.assertConfigureSucceeded res
  ]
  where
    configure = cabal_configure dir cabalPath
    withTestDir = withTempDir $ dir </> testDir
