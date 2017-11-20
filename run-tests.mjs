// -*- javascript-mode -*-
import { rt, js_from_scheme } from './rt/rt';

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

  const instance = await WebAssembly.instantiate(wasm, { 'rt': rt });

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
