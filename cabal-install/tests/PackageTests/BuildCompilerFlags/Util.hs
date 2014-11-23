module PackageTests.BuildCompilerFlags.Util where

import qualified PackageTests.PackageTester as PT

import Test.HUnit ( (@?), Assertion )
import Data.Maybe ( fromJust )
import Control.Applicative ( (<$>) )
import System.FilePath ( (</>) )
import System.Directory ( copyFile )

testHcPkg, testCompiler :: FilePath
testCompiler = "test-build-compiler.sh"
testHcPkg = "test-build-hc-pkg.sh"

testHcPkgPath, testCompilerPath :: PT.TestsPaths -> FilePath
testCompilerPath paths =
  fromJust (PT.tempDir paths) </> testCompiler
testHcPkgPath paths =
  fromJust (PT.tempDir paths) </> testHcPkg

testBuildCompOpt, testBuildHcPkgOpt :: PT.TestsPaths -> String
testBuildCompOpt paths =
  "--build-compiler-path=" ++ testCompilerPath paths
testBuildHcPkgOpt paths =
  "--build-hc-pkg-path=" ++ testHcPkgPath paths

-- | Copy 'testCompiler' and 'testHcPkg' to 'tempDir', so when they write their
-- output files they write them to the tests temp dir.
copyTestScriptsToTempDir :: PT.TestsPaths -> IO ()
copyTestScriptsToTempDir paths = do
  let sourceDir = "PackageTests" </> "BuildCompilerFlags"
  copyFile (sourceDir </> testCompiler) (testCompilerPath paths)
  copyFile (sourceDir </> testHcPkg) (testHcPkgPath paths)

cabalConfigBuildCompiler :: PT.TestsPaths -> String
cabalConfigBuildCompiler paths = "\
  \jobs: 1\n\
  \build-compiler-path: " ++ testCompilerPath paths ++ "\n\
  \build-hc-pkg-path:   " ++ testHcPkgPath paths

assertLineInFile :: String -> FilePath -> Assertion
assertLineInFile needle file =
  elem needle . lines <$> readFile file
  @? "File: \"" ++ file ++
  "\" should have contained this line: \""++needle++"\""

assertLineInString :: String -> String -> Assertion
assertLineInString needle content = (elem needle $ lines content)
  @? "The text: \"" ++ content ++
  "\" should have contained this line: \""++needle++"\""

assertTestCompilerLogFile :: PT.TestsPaths -> Assertion
assertTestCompilerLogFile paths =
  assertLineInFile "%% TEST BUILD COMPILER USED %%"
  (fromJust (PT.tempDir paths) </> "TEST_BUILD_COMPILER_OUTPUT")

assertTestHcPkgLogFile :: PT.TestsPaths -> Assertion
assertTestHcPkgLogFile paths =
  assertLineInFile "%% TEST BUILD HC PKG USED %%"
  (fromJust (PT.tempDir paths) </> "TEST_BUILD_HC_PKG_OUTPUT")
