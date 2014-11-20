module PackageTests.BuildCompilerFlags.Util where

import qualified PackageTests.PackageTester as PT

import Test.HUnit ( (@?), Assertion )
import Control.Exception (bracket)
import Control.Applicative ( (<$>) )
import Control.Monad (when)
import System.Directory ( removeDirectoryRecursive
                        , createDirectory
                        , doesDirectoryExist )
import System.FilePath ( (</>) )

testHcPkg, testCompiler, cabalConfigFile, buildDir, testDir :: FilePath
buildDirOpt, cabalConfigFileOpt, cabalConfigBuildCompiler, testBuildCompOpt,
  testBuildHcPkgOpt :: String

testDir            = "test-dir"
buildDir           = testDir </> "dist"
buildDirOpt        = "--builddir=" ++ buildDir
cabalConfigFile    = testDir </> "cabal-config"
cabalConfigFileOpt = "--config-file=" ++ cabalConfigFile
testCompiler       = "../test-build-compiler.sh"
testBuildCompOpt   = "--build-compiler-path=" ++ testCompiler
testHcPkg          = "../test-build-hc-pkg.sh"
testBuildHcPkgOpt  = "--build-hc-pkg-path=" ++ testHcPkg

cabalConfigBuildCompiler = "\
  \jobs: 1\n\
  \build-compiler-path: " ++ testCompiler ++ "\n\
  \build-hc-pkg-path:   " ++ testHcPkg

cabal_configure :: FilePath -> FilePath -> [String] -> IO PT.Result
cabal_configure wDir cabalPath configureArgs =
  let
    configureArgs' = buildDirOpt : configureArgs
    globalArgs     = [cabalConfigFileOpt]
  in
   PT.cabal_configure wDir globalArgs configureArgs' cabalPath

assertLineInFile :: String -> FilePath -> Assertion
assertLineInFile needle file =
  elem needle . lines <$> readFile file
  @? "File: \"" ++ file ++
  "\" should have contained this line: \""++needle++"\""

assertLineInString :: String -> String -> Assertion
assertLineInString needle content = (elem needle $ lines content)
  @? "The text: \"" ++ content ++
  "\" should have contained this line: \""++needle++"\""


withTempDir :: FilePath -> IO a -> IO a
withTempDir d = bracket createTestDir removeDirectoryRecursive . const
  where
    createTestDir = do
      testDirExists <- doesDirectoryExist d
      when testDirExists $
        removeDirectoryRecursive d
      createDirectory d
      return d

assertTestCompilerLogFile :: FilePath -> Assertion
assertTestCompilerLogFile wDir =
  assertLineInFile "%% TEST BUILD COMPILER USED %%"
  (wDir </> testDir </> "TEST_BUILD_COMPILER_OUTPUT")

assertTestHcPkgLogFile :: FilePath -> Assertion
assertTestHcPkgLogFile wDir =
  assertLineInFile "%% TEST BUILD HC PKG USED %%"
  (wDir </> testDir </> "TEST_BUILD_HC_PKG_OUTPUT")
