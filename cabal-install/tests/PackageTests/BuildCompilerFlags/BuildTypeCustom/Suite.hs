module PackageTests.BuildCompilerFlags.BuildTypeCustom.Suite ( test ) where

import           PackageTests.BuildCompilerFlags.Util
import qualified PackageTests.PackageTester as PT

import           System.FilePath ((</>))
import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

dir :: FilePath
dir = "PackageTests" </> "BuildCompilerFlags" </> "BuildTypeCustom"

test :: PT.TestsPaths -> Test
test paths' =
  testGroup "BuildTypeCustom"
  [
    testGroup "'cabal configure'"
    [
      testCase
      "Add default setup compiler options to newly generated ~/.cabal/config" $
      withTestDir $ \ paths -> do
        _ <- configure paths []
        assertLineInFile "-- build-compiler-path:" (PT.configPath paths)
        assertLineInFile "-- build-hc-pkg-path:" (PT.configPath paths)

    , testCase
      "Pass setup compiler options from ~/.cabal/config to 'Setup.hs'" $
      withTestDir $ \ paths -> do
        copyTestScriptsToTempDir paths
        writeFile (PT.configPath paths) (cabalConfigBuildCompiler paths)
        res <- configure paths []
        PT.assertConfigureSucceeded res
        let configureOutput = PT.outputText res
        assertLineInString (testBuildCompOpt paths) configureOutput
        assertLineInString (testBuildHcPkgOpt paths) configureOutput
        assertTestCompilerLogFile paths
        assertTestHcPkgLogFile paths

    , testCase
      "Pass command line setup compiler options to 'Setup.hs'" $
      withTestDir $ \ paths -> do
        copyTestScriptsToTempDir paths
        let opts = [testBuildCompOpt paths, testBuildHcPkgOpt paths]
        res <- configure paths opts
        PT.assertConfigureSucceeded res
        let configureOutput = PT.outputText res
        assertLineInString (testBuildCompOpt paths) configureOutput
        assertLineInString (testBuildHcPkgOpt paths) configureOutput
        assertTestCompilerLogFile paths
        assertTestHcPkgLogFile paths
    ]
  , testGroup "'cabal install'"
    [
      testCase
      "Pass setup compiler options from ~/.cabal/config to 'Setup.hs'" $
      withTestDir $ \ paths -> do
        copyTestScriptsToTempDir paths
        writeFile (PT.configPath paths) (cabalConfigBuildCompiler paths)
        res <- install paths []
        PT.assertInstallSucceeded res
        let installOutput = PT.outputText res
        assertLineInString (testBuildCompOpt paths) installOutput
        assertLineInString (testBuildHcPkgOpt paths) installOutput
        assertTestCompilerLogFile paths
        assertTestHcPkgLogFile paths

    , testCase
      "Use the '--with-build-*' command line options" $
      withTestDir $ \ paths -> do
        copyTestScriptsToTempDir paths
        let opts = [testBuildCompOpt paths, testBuildHcPkgOpt paths]
        res <- install paths opts
        PT.assertInstallSucceeded res
        let installOutput = PT.outputText res
        assertLineInString (testBuildCompOpt paths) installOutput
        assertLineInString (testBuildHcPkgOpt paths) installOutput
        assertTestCompilerLogFile paths
        assertTestHcPkgLogFile paths

    ]
  ]
  where
    configure p = PT.cabal_configure p dir
    install p = PT.cabal_install p dir
    withTestDir = PT.withTempBuildDir dir paths'
