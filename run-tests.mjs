// -*- javascript -*-
import * as Schism from './rt/rt';

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

    const engine = new Schism.Engine;
    const wasm = await engine.loadWasmModule(file);

    // set up the input port
    const input_file = name.replace(".ss", ".input");
    if (fs.existsSync(input_file)) {
	engine.setCurrentInputPort(fs.readFileSync(input_file));
    }

    let raw_result;
    let result;
    try {
	raw_result = wasm.exports['do-test']();
	result = engine.jsFromScheme(raw_result);
    } catch (e) {
	console.error(e.stack);
	throw e;
    }
    //console.info(raw_result);
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

async function createSchismFromWasm(schism_bytes) {
    return async function(name) {
	let engine = new Schism.Engine;
	let schism = await engine.loadWasmModule(schism_bytes);
	engine.setCurrentInputPort(fs.readFileSync(name));
	engine.clearOutputBuffer();
	schism.exports['compile-stdin->stdout']();

	const compiled_bytes = new Uint8Array(engine.output_data);
	fs.writeFileSync('out.wasm', compiled_bytes);
	return compiled_bytes;
    };
}

const use_stage0 = true;
const compileWithWasmScheme = (async function() {
  const schism_bytes = use_stage0 ? fs.readFileSync('schism-stage0.wasm') : await compileBootstrap();
  return createSchismFromWasm(schism_bytes);
}());

const use_host_compiler = false;
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
