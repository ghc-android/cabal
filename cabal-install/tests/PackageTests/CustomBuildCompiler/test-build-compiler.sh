#!/bin/bash

echo "%% CUSTOM BUILD COMPILER USED %%" > test-dir/TEST_BUILD_COMPILER_MARKER


ghc $@
