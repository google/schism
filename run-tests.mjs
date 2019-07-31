// -*- javascript -*-
//
// Copyright 2018, 2019 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import * as Schism from './rt/rt.mjs';
import { compileWithHostScheme, OPTIONS,
    stage0_bytes, stage0_compile,
    stage1_bytes, stage1_compile,
    stage2_bytes, stage2_compile,
    stage3_bytes } from './run-utils.mjs';

import assert from 'assert';
import fs from 'fs';
import process from 'process';
import util from 'util';

import * as addNumsTest from './test/add-nums-arg.mjs';
import * as eqFalseTest from './test/eq-false-arg.mjs';
import * as evalTest from './test/eval.mjs';

const testModules = {
    'test/add-nums-arg.ss': addNumsTest,
    'test/eq-false-arg.ss': eqFalseTest,
    'test/eval.ss': evalTest
};

Error.stackTraceLimit = 20;

async function runTest(name, compile = compileWithHostScheme) {
    const bytes = fs.readFileSync(name);
    const file = await compile(bytes);
    fs.writeFileSync('test.wasm', file);

    const engine = new Schism.Engine;
    const wasm = await engine.loadWasmModule(file);

    // set up the input port
    const input_file = name.replace(".ss", ".input");
    if (fs.existsSync(input_file)) {
        engine.setCurrentInputPort(fs.readFileSync(input_file));
    }

    // Set up mjs tests
    const testModule = testModules[name];

    try {
        if(testModule) {
            testModule.test(wasm, engine, assert);
        } else {
            const result = wasm.call('do-test');
            assert.ok(result != false, "test failed");
        }
    } catch (e) {
        console.error(e.stack);
        throw e;
    }
}

async function getTests() {
    if (process.argv.length > 2) {
        return process.argv.slice(2);
    } else {
        let files = await util.promisify(fs.readdir)('test');
        return (function* () {
            for (const name of files) {
                yield `test/${name}`;
            }
        })();
    }
}

async function runTests() {
    let failures = [];
    const files = await getTests();
    for (const test of files) {
        if (test.endsWith(".ss")) {
            let local_failures = [];
            console.info(`Running test ${test}`);
            async function run_stage(name, compile) {
                try {
                    await runTest(test, compile);
                    console.info(`  ${name} succeeded`);
                } catch (e) {
                    console.info(`  ${name} FAILED`);
                    console.info(e.stack);
                    local_failures.push([name]);
                }
            }
            if (OPTIONS.stage0) {
                await run_stage("stage0", stage0_compile);
            }
            if (OPTIONS.stage1) {
                await run_stage("stage1", stage1_compile);
            }
            if (OPTIONS.stage2) {
                await run_stage("stage2", stage2_compile);
            }
            if (local_failures.length > 0) {
                failures.push([test, local_failures]);
            }
        }
    }
    return failures;
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

// We should be at a fixpoint by now, so we compile stage3 only to check for equality.
async function checkCompilerFixpoint() {
    const stage2 = await stage2_bytes;
    const stage3 = await stage3_bytes;
    assert.equal(stage2.length, stage3.length);
    for(const i in stage2) {
        assert.equal(stage2[i], stage3[i], `stage2 and stage3 compilers differ at byte ${i}`);
    }
}

let results = runTests();
if (OPTIONS.stage3) {
    checkCompilerFixpoint().catch((e) => { throw e; });
}
results.then((failures) => {
    if (failures.length > 0) {
        console.info("Some tests failed:");
        for (const failure of failures) {
            console.info(`    '${failure[0]}': ${failure[1].join(', ')}`);
        }
        process.exit(1);
    }
});
