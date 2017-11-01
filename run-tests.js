#!/usr/bin/env node

const assert = require('assert');
const child_process = require('child_process');
const fs = require('fs');
const util = require('util')

function runTest(name) {
    child_process.exec(`./schism.ss ${name}`, (error, stdout, stderr) => {
	console.log(`stdout: ${stdout}`);
	console.log(`stderr: ${stderr}`);
	if (error) {
	    throw error;
	}

	const file = fs.readFileSync('out.wasm');
	const wasm = new WebAssembly.Module(file);
	console.log(util.inspect(wasm, {showHidden: false, depth: null}));

	WebAssembly.instantiate(wasm).then((instance) => {
	    console.log(util.inspect(instance, {showHidden: false, depth: null}));
	    const result = instance.exports['do-test']();
	    console.log(result);

	    assert.equal(result, 1, "test failed");
	});
    });
}

runTest('test/trivial.ss');
