'use strict'

const log = require('fancy-log')
const PluginError = require('plugin-error')
const prettierEslint = require('prettier-eslint')
const { Transform } = require('stream')
const map = (transform) => new Transform({ objectMode: true, transform })

module.exports = () => {
  const report = { changed: 0, unchanged: 0 }
  return map(format).on('finish', () => {
    if (report.changed > 0) {
      const changed = 'formatted '
        .concat(report.changed)
        .concat(' file')
        .concat(report.changed === 1 ? '' : 's')
      const unchanged = 'left '
        .concat(report.unchanged)
        .concat(' file')
        .concat(report.unchanged === 1 ? '' : 's')
        .concat(' unchanged')
      log(`prettier-eslint: ${changed}; ${unchanged}`)
    } else {
      log(`prettier-eslint: left ${report.unchanged} file${report.unchanged === 1 ? '' : 's'} unchanged`)
    }
  })

  function format (file, enc, next) {
    if (file.isNull()) return next()
    if (file.isStream()) return next(new PluginError('gulp-prettier-eslint', 'Streaming not supported'))

    const input = file.contents.toString()
    const output = prettierEslint({ text: input, filePath: file.path })

    if (input === output) {
      report.unchanged += 1
    } else {
      report.changed += 1
      file.contents = Buffer.from(output)
    }

    next(null, file)
  }
}
