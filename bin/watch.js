const chokidar = require('chokidar')
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const browserSync = require("browser-sync").create();
const chalk = require("chalk")

const logPrefix = "[" + chalk.blue("Developsync") + "]"
const log = console.log.bind(console, logPrefix)

const SOURCE_DIR = "./src"
const dotFilesRegex = /(^|[\/\\])\../

const watcher = chokidar.watch(SOURCE_DIR, {
  ignored: dotFilesRegex,
  persistent: true
});


const startDevServer = async (log) => {
  log("Starting Dev Server...")

  // Watch and Build
  const buildApp = async () => {
    log("Building app...")
    try {
      const { stdout, stderr } = await exec('bin/build.sh');
      log(stdout)
      if (stderr && stderr.length > 0) {
        log(stderr);
      }
    } catch (error) {
      const { stderr } = error
      log(stderr)
    }
  }

  await buildApp()

  const allEvents = 'all'
  watcher.on(allEvents, async (event, path) => {
    log(event, path)
    await buildApp()
  })

  // BrowserSync
  browserSync.watch("./build/static/js/*.js").on("change", () => {
    log("Reloading js...")
    browserSync.reload()
  })

  browserSync.watch("./build/static/css/*.css").on("change", () => {
    log("Reloading css...")
    browserSync.reload()
  })

  browserSync.init({
    open: false,
    server: "./build"
  })
}

startDevServer(log)
