'use strict'

const prettier = require('../lib/gulp-prettier-eslint')
const vfs = require('vinyl-fs')

module.exports = (files) => () =>
  vfs
    .src(files)
    .pipe(prettier())
    .pipe(vfs.dest((file) => file.base))
