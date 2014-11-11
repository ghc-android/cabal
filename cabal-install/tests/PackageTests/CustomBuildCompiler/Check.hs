module PackageTests.CustomBuildCompiler.Check ( test ) where

import qualified PackageTests.PackageTester as PT

import           Test.Framework (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit ((@?), Assertion )

import           Control.Applicative ((<$>))
import           Control.Exception (bracket)
import           Control.Monad (when)
import           Data.List (isInfixOf)
import           System.Directory (doesFileExist, removeFile)
import           System.FilePath ((</>))

test :: FilePath -> Test
test cabalPath =
  testGroup "ConfigFileEntries"
  [ testCase "generate cabal config file entries for build compiler" $
    withNewCabalConfigFile $ \ cfgFile -> do
      cabal_configure ["--config-file=" ++ cfgFile] []
        >>= PT.assertConfigureSucceeded
      assertLineInFile "-- build-compiler-path:" (dir </> cfgFile)
      assertLineInFile "-- build-hc-pkg-path:" (dir </> cfgFile)

  , testCase "use the build compiler defined in the config file" $
    withNewCabalConfigFile $ \ cfgFile -> do
      cfgHead <- readFile configFileTemplate
      let cfg = cfgHead ++ "\nbuild-compiler-path: " ++ testBuildCompiler ++ "\n"
      writeFile (dir </> cfgFile) cfg
      result <- cabal_configure ["--config-file=" ++ cfgFile] []
      PT.assertConfigureSucceeded result
      buildCompilerMarkerString `isInfixOf` (PT.outputText result)
        @? "should have used the build compiler settings from config file"
  ]
  where
    cabal_configure globalArgs configureArgs =
      PT.cabal_configure dir globalArgs configureArgs cabalPath

dir :: FilePath
dir = "PackageTests" </> "CustomBuildCompiler"

configFileTemplate :: FilePath
configFileTemplate = dir </> "with-custom-build-compiler.cfg"

testBuildCompiler :: FilePath
testBuildCompiler = dir </> "test-build-compiler.sh"

buildCompilerMarkerString :: String
buildCompilerMarkerString = "%% CUSTOM BUILD COMPILER USED %%"

assertLineInFile :: String -> FilePath -> Assertion
assertLineInFile needle file =
  elem needle . lines <$> readFile file
  @? "File: \"" ++ file ++ "\" should have contained a line: \""++needle++"\""

withNewCabalConfigFile :: (FilePath -> IO a) -> IO a
withNewCabalConfigFile =
  bracket (cfgFile >>= ensureAbsent) (removeFile . (dir </>))
  where cfgFile = return "test-config"
        ensureAbsent cfgFile' = do
          fileExists <- doesFileExist $ dir </> cfgFile'
          when fileExists $ removeFile $ dir </> cfgFile'
          return cfgFile'
