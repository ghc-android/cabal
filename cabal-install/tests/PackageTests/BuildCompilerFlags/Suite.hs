module PackageTests.BuildCompilerFlags.Suite ( test ) where
-- Tests for custom build ('Setup.lhs') compiler options.
--
-- Test the propagation, delegation and persistance of 'configure' flags
-- controlling how the build/setup compiler is configured.
--
-- Tests either relate to 'custom' or 'simple' cabal projects.

import qualified PackageTests.BuildCompilerFlags.BuildTypeCustom.Suite as Custom

import qualified PackageTests.BuildCompilerFlags.BuildTypeSimple.Suite as Simple

-- Subdirectory 'BuildTypeCustom' contains a 'custom' cabal project, with a
-- special 'Setup.hs' that not invoke 'defaultMain', but instead echoes the
-- command line parameters into a text file examined by tests.
--
-- Subdirectory 'BuildTypeSimple' contains a 'simple' cabal project.

import Test.Framework (Test, testGroup)

test :: FilePath -> Test
test cabalPath =

  testGroup "BuildCompilerFlags"

  [ Custom.test cabalPath
  , Simple.test cabalPath
  ]
