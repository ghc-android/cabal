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
  testGroup "BuildTypeSimple"
  [
    testCase
    "Use the build compiler defined by 'configure' when executing the 'install' command" $
    withTestDir $ \ paths -> do
      copyTestScriptsToTempDir paths
      let opts = [testBuildCompOpt paths, testBuildHcPkgOpt paths]
      confRes <- configure paths opts
      PT.assertConfigureSucceeded confRes

      instRes <- install paths ["-j4"] -- enforce compilation of Setup.hs
      let installOutput = PT.outputText instRes

      assertLineInString (testBuildCompOpt paths) installOutput
      assertLineInString (testBuildHcPkgOpt paths) installOutput
      assertTestCompilerLogFile paths
      assertTestHcPkgLogFile paths
  ]
  where
    configure p = PT.cabal_configure p dir
    install p = PT.cabal_install p dir
    withTestDir = PT.withTempBuildDir dir paths'
