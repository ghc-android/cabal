#!/bin/bash

echo "%% CUSTOM BUILD HC PKG USED %%" > test-dir/TEST_BUILD_HC_PKG_OUTPUT

ghc-pkg $@ | tee -a test-dir/TEST_BUILD_HC_PKG_OUTPUT
