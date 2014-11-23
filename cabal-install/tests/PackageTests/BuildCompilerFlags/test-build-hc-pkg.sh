#!/bin/bash

TESTDIR=$(dirname "${0##}")

echo "%% TEST BUILD HC PKG USED %%" > ${TESTDIR}/TEST_BUILD_HC_PKG_OUTPUT

ghc-pkg $@ | tee -a ${TESTDIR}/TEST_BUILD_HC_PKG_OUTPUT
