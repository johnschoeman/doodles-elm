#!/bin/bash

set -e

# Setup
rm -rf build
mkdir build
cp -r public/* build

doodles=( modular_times_table black_sheep_jump lock_puzzle recaman dots squares mothers_day ortho_board )

for i in "${doodles[@]}"
do
  echo Building: "$i"

  cd src/$i
  bin/build.sh
  cd ../..

  cp -r ./src/$i/build/* build/
done
