#!/usr/bin/env node

const fs = require('fs');

let file = fs.readFileSync('out.wasm');
let wasm = new WebAssembly.Module(file);

const util = require('util')
console.log(util.inspect(wasm, {showHidden: false, depth: null}));
