const path = require('path')
const ElmPlugin = require('esbuild-plugin-elm')
const esbuild = require('esbuild')

esbuild.build({
  entryPoints: ['./src/index.js'], 
  bundle: true,
  outdir: "./build/static/js",
  watch: process.argv.includes("--watch"),
  sourcemap: process.argv.includes("--sourcemap"),
  minify: process.argv.includes("--minify"),
  plugins: [
    ElmPlugin()
  ],
}).catch(e => (console.error(e), process.exit(1)))
