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

	WebAssembly.instantiate(wasm).then((instance) => {
	    const result = instance.exports['do-test']();

	    assert.equal(result, 1, "test failed");
	});
    });
}

async function runTests() {
    const files = await util.promisify(fs.readdir)('test');
    for (const test of files) {
	if (test.endsWith(".ss")) {
	    console.info(`Running test ${test}`);
	    runTest(`test/${test}`);
	}
    }
}

runTests();
