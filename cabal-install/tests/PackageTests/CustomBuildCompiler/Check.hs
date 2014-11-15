module PackageTests.CustomBuildCompiler.Check ( test ) where

import qualified PackageTests.PackageTester as PT

import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit ((@?), Assertion )

import           Control.Applicative ((<$>))
import           Control.Exception (bracket, try)
import           Control.Monad (when)
import           Data.List (isInfixOf)
import           System.Directory (removeDirectoryRecursive, createDirectory, doesDirectoryExist)
import           System.FilePath ((</>))

test :: FilePath -> Test
test cabalPath =
  testGroup "CustomBuildCompiler"
  [ testCase "generate cabal config file entries for build compiler" $
    withEmptyTestDir $ do
      let cfgFile = testDir </> "test-config"
      cabal_configure ["--config-file=" ++ cfgFile] []
        >>= PT.assertConfigureSucceeded
      assertLineInFile "-- build-compiler-path:" (dir </> cfgFile)
      assertLineInFile "-- build-hc-pkg-path:" (dir </> cfgFile)

  , testCase "use the build compiler defined in the config file" $
    withEmptyTestDir $ do
      let cfgFile = testDir </> "test-config"
          cfg = configFileHead ++ "\nbuild-compiler-path: " ++ testBuildCompiler ++ "\n"
      writeFile (dir </> cfgFile) cfg
      result <- cabal_configure ["--config-file=" ++ cfgFile] []
      PT.assertConfigureSucceeded result
      markerFileContent <- readFile markerFile
      buildCompilerMarkerString `isInfixOf` markerFileContent
        @? "should have used the build compiler defined in the config file"
  ]
  where
    cabal_configure globalArgs configureArgs =
      PT.cabal_configure dir globalArgs configureArgs cabalPath

dir :: FilePath
dir = "PackageTests" </> "CustomBuildCompiler"

testDir :: FilePath
testDir = "test-dir"

configFileHead :: FilePath
configFileHead =
  "remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive\n\
  \remote-repo-cache: /home/sven/.cabal/packages\n\
  \world-file: /home/sven/.cabal/world\n\
  \extra-prog-path: /home/sven/.cabal/bin\n\
  \build-summary: /home/sven/.cabal/logs/build.log\n\
  \remote-build-reporting: anonymous\n\
  \jobs: $ncpus\n\n"

testBuildCompiler :: FilePath
testBuildCompiler = "./test-build-compiler.sh"

markerFile :: FilePath
markerFile = "TEST_BUILD_COMPILER_MARKER"

buildCompilerMarkerString :: String
buildCompilerMarkerString = "%% CUSTOM BUILD COMPILER USED %%"

assertLineInFile :: String -> FilePath -> Assertion
assertLineInFile needle file =
  elem needle . lines <$> readFile file
  @? "File: \"" ++ file ++ "\" should have contained a line: \""++needle++"\""

withEmptyTestDir :: IO a -> IO a
withEmptyTestDir =
  bracket createTestDir removeDirectoryRecursive . const
  where
    createTestDir = do
      let testDirCwd = dir </> testDir
      doesDirectoryExist testDirCwd >>= flip when (removeDirectoryRecursive testDirCwd)
      createDirectory testDirCwd
      return testDirCwd
