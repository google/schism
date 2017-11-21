// -*- javascript-mode -*-
import { rt, js_from_scheme, set_current_input_port } from './rt/rt';

import assert from 'assert';
import child_process from 'child_process';
import fs from 'fs';
import util from 'util';

async function runTest(name) {
  const { stdout, stderr } = await util.promisify(child_process.exec)(`./schism.ss ${name}`);

  console.log(`stdout: ${stdout}`);
  console.log(`stderr: ${stderr}`);

  const file = fs.readFileSync('out.wasm');
  const wasm = new WebAssembly.Module(file);

  // set up the input port
  const input_file = name.replace(".ss", ".input");
  if (fs.existsSync(input_file)) {
	set_current_input_port(fs.readFileSync(input_file));
  } else {
	set_current_input_port([]);
  }

  const instance = await WebAssembly.instantiate(wasm, { 'rt': rt });

  const result = js_from_scheme(instance.exports['do-test']());
  assert.ok(result != false, "test failed");
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
