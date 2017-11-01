#!/usr/bin/env node

const assert = require('assert');
const child_process = require('child_process');
const fs = require('fs');
const util = require('util')

async function runTest(name) {
    const { stdout, stderr } = await util.promisify(child_process.exec)(`./schism.ss ${name}`);
    
    console.log(`stdout: ${stdout}`);
    console.log(`stderr: ${stderr}`);
    
    const file = fs.readFileSync('out.wasm');
    const wasm = new WebAssembly.Module(file);

    const instance = await WebAssembly.instantiate(wasm);

    const result = instance.exports['do-test']();
	
    assert.equal(result, 1, "test failed");
}

async function runTests() {
    const files = await util.promisify(fs.readdir)('test');
    for (const test of files) {
	if (test.endsWith(".ss")) {
	    console.info(`Running test ${test}`);
	    await runTest(`test/${test}`);
	}
    }
}

runTests();
