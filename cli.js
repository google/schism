// This file is necessary to be CommonJS because meow depends on `module.parent`
// which doesn't exist if the parent is an ES module.

const meow = require('meow');

const cli = meow(`
  Usage
    $ schism <input>

  Options
    --version             Show the version number
    --help                Show the help message
    -o, --out [out.wasm]  Specify a file to write the wasm to
    --stage [0]           Specify which stage compiler to use
`, {
  flags: {
    out: {
      type: 'string',
      alias: 'o',
      default: 'out.wasm'
    },
    stage: {
      type: 'string',
      default: '0'
    }
  }
});

module.exports = cli;
