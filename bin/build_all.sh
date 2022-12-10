set -e

# Setup
rm -rf build
mkdir build
cp -r public/* build

# Build
cd src/modular_times_table
bin/build.sh
cd ../..

cd src/black_sheep_jump
bin/build.sh
cd ../..

cd src/lock_puzzle
bin/build.sh
cd ../..

cd src/recaman
bin/build.sh
cd ../..

cd src/recaman
bin/build.sh
cd ../..

cp -r ./src/modular_times_table/build/* build/
cp -r ./src/black_sheep_jump/build/* build/
cp -r ./src/lock_puzzle/build/* build/
cp -r ./src/recaman/build/* build/
