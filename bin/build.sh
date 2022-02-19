set -e

# Setup
rm -rf build
mkdir build
cp -r public/* build

# Build
npx tailwindcss -i ./src/tailwind.css -o ./build/static/css/index.css
node bin/esbuild.js --sourcemap
