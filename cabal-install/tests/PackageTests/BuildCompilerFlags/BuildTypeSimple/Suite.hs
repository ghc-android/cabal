module PackageTests.BuildCompilerFlags.BuildTypeSimple.Suite ( test ) where

import PackageTests.BuildCompilerFlags.Util
import qualified PackageTests.PackageTester as PT

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import System.FilePath ((</>))

dir :: FilePath
dir = "PackageTests" </> "BuildCompilerFlags" </> "BuildTypeSimple"

test :: PT.TestsPaths -> Test
test paths =
  testGroup "BuildTypeSimple"
  [
    testCase
    "Use the build compiler defined by 'configure' when executing the 'install' command" $
    withTestDir $ do
      confRes <- configure [testBuildCompOpt, testBuildHcPkgOpt]
      PT.assertConfigureSucceeded confRes

      instRes <- install ["-j", "4"] -- enforce compilation of Setup.hs
      let installOutput = PT.outputText instRes

      assertLineInString testBuildCompOpt installOutput
      assertLineInString testBuildHcPkgOpt installOutput
      assertTestCompilerLogFile dir
      assertTestHcPkgLogFile dir
  ]
  where
    configure = cabal_configure paths dir
    install = PT.cabal_install paths dir
    withTestDir = withTempDir $ dir </> testDir
