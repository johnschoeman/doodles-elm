const ElmPlugin = require('esbuild-plugin-elm')
const esbuild = require('esbuild')

const doodle = process.argv[2]
const outdir = `./build/${doodle}`

esbuild.build({
  entryPoints: ["./src/index.js"],
  bundle: true,
  outdir,
  watch: process.argv.includes("--watch"),
  sourcemap: process.argv.includes("--sourcemap"),
  minify: process.argv.includes("--minify"),
  plugins: [
    ElmPlugin()
  ],
}).catch(() => process.exit(1))
