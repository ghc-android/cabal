module PackageTests.CustomBuildCompiler.Check ( test ) where

import qualified PackageTests.PackageTester as PT

import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit ((@?), Assertion )

import           Control.Applicative ((<$>))
import           Control.Exception (bracket)
import           Control.Monad (when)
import           Data.List (isInfixOf)
import           Distribution.Simple.Configure ( getPersistBuildConfig )
import           System.Directory ( canonicalizePath
                                  , removeDirectoryRecursive
                                  , createDirectory
                                  , doesDirectoryExist )
import           System.FilePath ((</>))

-- TODO sh: Split this test in tests for the compilation and invokation of
-- Setup.hs, and test starting from 'Distribution.Simple.defaultMain'. The
-- 'Setup.hs' contained in this test does not invoke 'defaultMain', it merely
-- asserts that the options are correct. The other tests belong to Cabal's test
-- suite, not in cabal-install's test suite, and start by calling defaultMain
-- and go from there.

-- TODO sh: Add a build type 'Simple' test project.


test :: FilePath -> Test
test cabalPath =
  testGroup "CustomBuildCompiler"
  [ testCase
    "Add default setup compiler options to newly generated ~/.cabal/config" $
    withEmptyTestDir $ do
      _ <- cabal_configure []
      assertLineInFile "-- build-compiler-path:" (dir </> cabalConfigFile)
      assertLineInFile "-- build-hc-pkg-path:" (dir </> cabalConfigFile)

  , testCase
    "Pass setup compiler options from ~/.cabal/config to 'Setup.hs'" $
    withEmptyTestDir $ do
      writeFile (dir </> cabalConfigFile) cabalConfigCustomSetup
      res <- cabal_configure []
      PT.assertConfigureSucceeded res
      let configureOutput = PT.outputText res
      assertLineIn customCompilerOpt configureOutput
      assertLineIn customPkgOpt configureOutput
      assertCustomCompilerMarkerFile
      assertCustomHcPkgMarkerFile

  , testCase
    "Pass command line setup compiler options to 'Setup.hs'" $
    withEmptyTestDir $ do
      res <- cabal_configure [customCompilerOpt, customPkgOpt]
      PT.assertConfigureSucceeded res
      let configureOutput = PT.outputText res
      assertLineIn customCompilerOpt configureOutput
      assertLineIn customPkgOpt configureOutput
      assertCustomCompilerMarkerFile
      assertCustomHcPkgMarkerFile
  ]
  where
    cabal_configure configureArgs =
      let configureArgs' = ("--builddir=" ++ buildDir) : configureArgs
          globalArgs = ["--config-file=" ++ cabalConfigFile]
      in PT.cabal_configure dir globalArgs configureArgs' cabalPath

dir :: FilePath
dir = "PackageTests" </> "CustomBuildCompiler"

testDir :: FilePath
testDir = "test-dir"

buildDir :: FilePath
buildDir = testDir </> "dist"

cabalConfigFile :: FilePath
cabalConfigFile = testDir </> "cabal-config"

cabalConfig :: String
cabalConfig =
  "remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive\n\
  \remote-repo-cache: /home/sven/.cabal/packages\n\
  \world-file: /home/sven/.cabal/world\n\
  \extra-prog-path: /home/sven/.cabal/bin\n\
  \build-summary: /home/sven/.cabal/logs/build.log\n\
  \remote-build-reporting: anonymous\n\
  \jobs: $ncpus\n\n"

cabalConfigCustomSetup :: String
cabalConfigCustomSetup =
  cabalConfig ++ "\n\
  \\n\
  \build-compiler-path: " ++ customCompiler ++ "\n\
  \build-hc-pkg-path:   " ++ customPkg      ++ "\n\
  \\n"

customCompiler :: String
customCompiler = "./test-build-compiler.sh"

customCompilerOpt :: String
customCompilerOpt = "--build-compiler-path=" ++ customCompiler

customPkg :: String
customPkg = "./test-build-hc-pkg.sh"

customPkgOpt :: String
customPkgOpt = "--build-hc-pkg-path=" ++ customPkg

assertLineInFile :: String -> FilePath -> Assertion
assertLineInFile needle file =
  elem needle . lines <$> readFile file
  @? "File: \"" ++ file ++ "\" should have contained this line: \""++needle++"\""

assertLineIn :: String -> String -> Assertion
assertLineIn needle content = (elem needle $ lines content)
  @? "The text: \"" ++ content ++ "\" should have contained this line: \""++needle++"\""

assertCustomCompilerMarkerFile :: Assertion
assertCustomCompilerMarkerFile = assertLineInFile
                                 "%% CUSTOM BUILD COMPILER USED %%"
                                 (dir </> testDir </> "TEST_BUILD_COMPILER_OUTPUT")

assertCustomHcPkgMarkerFile :: Assertion
assertCustomHcPkgMarkerFile = assertLineInFile
                              "%% CUSTOM BUILD HC PKG USED %%"
                              (dir </> testDir </> "TEST_BUILD_HC_PKG_OUTPUT")


withEmptyTestDir :: IO a -> IO a
withEmptyTestDir = bracket createTestDir return . const
  where
    createTestDir = do
      let testDirCwd = dir </> testDir
      doesDirectoryExist testDirCwd >>= flip when (removeDirectoryRecursive testDirCwd)
      createDirectory testDirCwd
      return testDirCwd
