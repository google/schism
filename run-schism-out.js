#!/usr/bin/env node

const fs = require('fs');

const file = fs.readFileSync('out.wasm');
const wasm = new WebAssembly.Module(file);

const util = require('util')
console.log(util.inspect(wasm, {showHidden: false, depth: null}));

WebAssembly.instantiate(wasm).then((instance) => {
  console.log(util.inspect(instance, {showHidden: false, depth: null}));
  console.log(instance.exports.return_5());
});
