#!/usr/bin/env bash

OUTDIR="$1"


cd /usr/src

STACK_ROOT=$OUTDIR/stackroot-default
STACK_WORK=$OUTDIR/stackwork-default

mkdir -p $(pwd)/$STACK_ROOT
mkdir -p $(pwd)/$STACK_WORK

STACK_CMD="stack --stack-root $(pwd)/$STACK_ROOT --work-dir $STACK_WORK"

echo "---------------------------"
echo "stack setup"
echo "---------------------------"
$STACK_CMD setup "$(ghc --numeric-version)" --skip-ghc-check --allow-different-user

echo "---------------------------"
echo "stack build"
echo "---------------------------"
$STACK_CMD clean --allow-different-user
$STACK_CMD build --allow-different-user --ghc-options="$GHC_OPTIONS" -- .

echo "---------------------------"
echo "install..."
echo "---------------------------"
$STACK_CMD --local-bin-path $OUTDIR install --allow-different-user --ghc-options "$GHC_OPTIONS" --test

# SHOW INFORMATION ABOUT output
ldd $OUTDIR/sgHomePage-exe || true
du -hs $OUTDIR
