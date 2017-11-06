#!/usr/bin/env node

const assert = require('assert');
const child_process = require('child_process');
const fs = require('fs');
const util = require('util')

const TAG_SIZE = 3;
const TAGS = {
    fixnum: 0,
    constant: 1
};

function tag_constant(value, tag) {
    return (value << TAG_SIZE) | tag;
}

function extract_tag(value) {
    return value & ((1 << TAG_SIZE) - 1);
}

function extract_value(value) {
    return value >>> TAG_SIZE;
}

const SCHEME_CONSTANTS = {
    0: false,
    1: true
}

// Convert a Scheme ptr into a corresponding JavaScript value
function js_from_scheme(ptr) {
    switch (extract_tag(ptr)) {
    case TAGS.fixnum:
	return ptr >> TAG_SIZE; // sign extending shift so negatives work.
    case TAGS.constant:
	return SCHEME_CONSTANTS[extract_value(ptr)];
    }
}

async function runTest(name) {
    const { stdout, stderr } = await util.promisify(child_process.exec)(`./schism.ss ${name}`);
    
    console.log(`stdout: ${stdout}`);
    console.log(`stderr: ${stderr}`);
    
    const file = fs.readFileSync('out.wasm');
    const wasm = new WebAssembly.Module(file);

    const instance = await WebAssembly.instantiate(wasm);

    const result = js_from_scheme(instance.exports['do-test']());
    
    assert.equal(result == false || result == 1, 1, "test failed");
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
