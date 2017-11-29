// -*- javascript -*-
import { rt, js_from_scheme, set_current_input_port, output_data } from './rt/rt';

import assert from 'assert';
import child_process from 'child_process';
import fs from 'fs';
import util from 'util';

// Returns the contents of out.wasm
async function compileWithHostScheme(name) {
    const { stdout, stderr } = await util.promisify(child_process.exec)(`./schism.ss ${name}`);

    console.log(`stdout: ${stdout}`);
    console.log(`stderr: ${stderr}`);

    return util.promisify(fs.readFile)('out.wasm');
}

// Uses host scheme to compile schism and returns the wasm bytes
async function compileBootstrap() {
    return compileWithHostScheme('./schism/compiler.ss');
}

async function runTest(name, compile = compileWithHostScheme) {
    const file = await compile(name);
    const wasm = new WebAssembly.Module(file);

    // set up the input port
    const input_file = name.replace(".ss", ".input");
    if (fs.existsSync(input_file)) {
	set_current_input_port(fs.readFileSync(input_file));
    } else {
	set_current_input_port([]);
    }

    const instance = await WebAssembly.instantiate(wasm, { 'rt': rt });

    let result;
    try {
	result = js_from_scheme(instance.exports['do-test']());
    } catch (e) {
	console.error(e.stack);
	throw e;
    }
    // console.info(result);
    assert.ok(result != false, "test failed");
}

async function runTests(compile = compileWithHostScheme) {
    const files = await util.promisify(fs.readdir)('test');
    for (const test of files) {
	if (test.endsWith(".ss")) {
	    console.info(`Running test ${test}`);
	    await runTest(`test/${test}`, compile);
	}
    }
}

const compileWithWasmScheme = (async function() {
    const schism_bytes = await compileBootstrap();

    return async function(name) {
	const schism = await WebAssembly.instantiate(schism_bytes, { 'rt': rt });
	set_current_input_port(fs.readFileSync(name));
	output_data.length = 0;
	schism.instance.exports['compile-stdin->stdout']();

	const compiled_bytes = new Uint8Array(output_data);
	fs.writeFileSync('out.wasm', compiled_bytes);
	return compiled_bytes;
    };
}());

const use_host_compiler = true;
if (use_host_compiler) {
    runTests().catch((e) => {
      console.error(e.stack);
      throw e;
    });
} else {
  compileWithWasmScheme.then((compile) => {
    runTests(compile).catch((e) => {
      console.error(e.stack);
      throw e;
    });
  });
}
