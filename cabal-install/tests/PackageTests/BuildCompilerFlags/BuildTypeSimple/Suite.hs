module PackageTests.BuildCompilerFlags.BuildTypeSimple.Suite ( test ) where

import PackageTests.BuildCompilerFlags.Util
import qualified PackageTests.PackageTester as PT

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import System.FilePath ((</>))

dir :: FilePath
dir = "PackageTests" </> "BuildCompilerFlags" </> "BuildTypeSimple"

test :: PT.TestsPaths -> Test
test paths' =
  testGroup "BuildTypeSimple - Smoke tests"
  [
    testCase
    "A 'simple' project can be configured and installed in the presence of '--with-build-*' flags" $
    withTestDir $ \ paths -> do
      copyTestScriptsToTempDir paths
      let opts = [testBuildCompOpt paths, testBuildHcPkgOpt paths]
      confRes <- PT.cabal_configure paths dir opts
      PT.assertConfigureSucceeded confRes
      instRes <- PT.cabal_install paths dir []
      PT.assertInstallSucceeded instRes
  ]
  where
    withTestDir = PT.withTempBuildDir dir paths'
