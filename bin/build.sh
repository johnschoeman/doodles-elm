# Setup
rm -rf build
mkdir build
mkdir -p build/static/css
cp -r public/* build

# Build
npx tailwindcss -i ./src/tailwind.css -o ./build/static/css/tailwind.css
node ./esbuild.js --minify=false --sourcemap=true
