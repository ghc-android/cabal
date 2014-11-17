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

-- TODO sh: There is a big problem (due to the fact that the cabal design suck
--          generally at this) there is a assymetric somewhat redundant split
--          between Setup.hs compilation and and the rest, well wtf whatever,
--          this results in Setup.hs not being linked to the (sandboxed?) local
--          Cabal lib. Solution: Split this test in tests for control flow only
--          until Setup.hs is compiled with the right compiler and invoked with
--          the provided flags, and test starting from
--          'Distribution.Simple.defaultMain'. Setup.hs in this project does not
--          invoke 'defaultMain', it merely asserts that stuff is correct.  The
--          second part of the tests is to be placed in Cabal's test suite, not
--          in cabal-install's test suite. These tests start by calling
--          defaultMain and go from there.


test :: FilePath -> Test
test cabalPath =
  testGroup "CustomBuildCompiler"
  [ testCase "Add setup compiler options to newly generated ~/.cabal/config" $
    withEmptyTestDir $ do
      let cfgFile = testDir </> "test-config"
      _ <- cabal_configure ["--config-file=" ++ cfgFile] []
      assertLineInFile "-- build-compiler-path:" (dir </> cfgFile)
      assertLineInFile "-- build-hc-pkg-path:" (dir </> cfgFile)

  , testCase "Read setup compiler options from ~/.cabal/config" $
    withEmptyTestDir $ do
      testBuildCompilerAbs <- canonicalizePath $ dir </> testBuildCompiler
      let cfgFile = testDir </> "test-config"
          cfg = configFileHead
                ++ "\nbuild-compiler-path: " ++ testBuildCompilerAbs
                ++ "\n"
      writeFile (dir </> cfgFile) cfg
      cabal_configure ["--config-file=" ++ cfgFile] []
        >>= PT.assertConfigureSucceeded
      markerFileContent <- readFile markerFile
      buildCompilerMarkerString `isInfixOf` markerFileContent
        @? "should have used the build compiler defined in the config file"

    , testCase "Write setup compiler options to LocalBuildInfo" $
      withEmptyTestDir $ do
      let persistentBuildCfgFile = buildDir </> ""
      cabal_configure [] ["--build-compiler-path=bla",
                          "--build-hc-pkg-path=blub"]
        >>= PT.assertConfigureSucceeded
      getPersistBuildConfig persistentBuildCfgFile >>= (mapM_ putStrLn . lines . show)
      return ()
    ]
  where
    cabal_configure globalArgs configureArgs =
      let configureArgs' = ("--builddir=" ++ buildDir) : configureArgs
      in PT.cabal_configure dir globalArgs configureArgs' cabalPath

dir :: FilePath
dir = "PackageTests" </> "CustomBuildCompiler"

testDir :: FilePath
testDir = "test-dir"

buildDir :: FilePath
buildDir = testDir </> "dist"

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
withEmptyTestDir = bracket createTestDir return . const
  where
    createTestDir = do
      let testDirCwd = dir </> testDir
      doesDirectoryExist testDirCwd >>= flip when (removeDirectoryRecursive testDirCwd)
      createDirectory testDirCwd
      return testDirCwd
