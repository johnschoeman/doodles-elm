set -e

DOODLE="black_sheep_jump"

# Setup
rm -rf build
mkdir -p build/$DOODLE
cp -r public/* build/$DOODLE

# Build
npx tailwindcss -i ./src/tailwind.css -o ./build/$DOODLE/index.css
node bin/esbuild.js $DOODLE
