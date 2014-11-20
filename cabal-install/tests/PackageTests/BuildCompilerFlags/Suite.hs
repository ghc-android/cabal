module PackageTests.BuildCompilerFlags.Suite ( test ) where
-- Tests for custom build ('Setup.lhs') compiler options.
--
-- Test the propagation, delegation and persistance of 'configure' flags
-- controlling how the build/setup compiler is configured.
--
-- Tests either relate to 'custom' or 'simple' cabal projects.

import qualified PackageTests.BuildCompilerFlags.BuildTypeCustom.Suite as Custom
-- 'BuildTypeCustom' contains a 'custom' cabal project, with a special
-- 'Setup.hs' that not invoke 'defaultMain', but instead echoes the command line
-- parameters into a text file examined by tests.
--
import qualified PackageTests.BuildCompilerFlags.BuildTypeSimple.Suite as Simple
-- 'BuildTypeSimple' contains a 'simple' cabal project.

import qualified PackageTests.PackageTester as PT
import Test.Framework (Test, testGroup)

test :: PT.TestsPaths -> Test
test paths =
  testGroup "BuildCompilerFlags"
  [
    Custom.test paths
  , Simple.test paths
  ]
