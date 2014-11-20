module PackageTests.BuildCompilerFlags.BuildTypeCustom.Suite ( test ) where

import PackageTests.BuildCompilerFlags.Util
import qualified PackageTests.PackageTester as PT

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import System.FilePath ((</>))

dir :: FilePath
dir = "PackageTests" </> "BuildCompilerFlags" </> "BuildTypeCustom"

test :: PT.TestsPaths -> Test
test paths =
  testGroup "BuildTypeCustom"
  [
    testCase
    "Add default setup compiler options to newly generated ~/.cabal/config" $
    withTestDir $ do
      _ <- configure []
      assertLineInFile "-- build-compiler-path:" (dir </> cabalConfigFile)
      assertLineInFile "-- build-hc-pkg-path:" (dir </> cabalConfigFile)

  , testCase
    "Pass setup compiler options from ~/.cabal/config to 'Setup.hs'" $
    withTestDir $ do
      writeFile (dir </> cabalConfigFile) cabalConfigBuildCompiler
      res <- configure []
      PT.assertConfigureSucceeded res
      let configureOutput = PT.outputText res
      assertLineInString testBuildCompOpt configureOutput
      assertLineInString testBuildHcPkgOpt configureOutput
      assertTestCompilerLogFile dir
      assertTestHcPkgLogFile dir

  , testCase
    "Pass command line setup compiler options to 'Setup.hs'" $
    withTestDir $ do
      res <- configure [testBuildCompOpt, testBuildHcPkgOpt]
      PT.assertConfigureSucceeded res
      let configureOutput = PT.outputText res
      assertLineInString testBuildCompOpt configureOutput
      assertLineInString testBuildHcPkgOpt configureOutput
      assertTestCompilerLogFile dir
      assertTestHcPkgLogFile dir
  ]
  where
    configure = cabal_configure paths dir
    withTestDir = withTempDir $ dir </> testDir
