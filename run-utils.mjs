// -*- javascript -*-
//
// Copyright 2018 Google LLC
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

import child_process from 'child_process';
import fs from 'fs';
import util from 'util';
import root from './root.mjs';

const compilerPath = root.join('schism/compiler.ss');

export const OPTIONS = {
    use_snapshot: true, // load schism-stage0.wasm if true instead of
                         // building with host scheme

    // which stages to build and run tests for
    stage0: true,
    stage1: true,
    stage2: true,
    stage3: true, // compile-only, no point running tests
}

// Returns the contents of out.wasm
export async function compileWithHostScheme(name) {
    const { stdout, stderr } = await util.promisify(child_process.exec)(`./schism.ss ${name}`);

    return util.promisify(fs.readFile)('out.wasm');
}

// Uses host scheme to compile schism and returns the wasm bytes
async function compileBootstrap() {
    return compileWithHostScheme('./schism/compiler.ss');
}

function make_compiler(compiler_bytes, name) {
    return async function(bytes, programName = "[unknown]") {
        //console.info(`Compiling ${programName} with ${name}`);
        const engine = new Schism.Engine;
        const schism = await engine.loadWasmModule(await compiler_bytes());
        engine.setCurrentInputPort(bytes);
        // let module_package = schism.exports['compile-stdin->module-package']();
        // module_package = engine.collect(module_package);
        // schism.exports['compile-module-package->stdout'](module_package);
        schism.exports['compile-stdin->stdout']();
        //console.info(`Done compiling ${programName} with ${name}`);
        return new Uint8Array(engine.output_data);
    }
}

function make_cache(thunk) {
  let cache = undefined;
  return () => {
    if (!cache) {
      cache = thunk();
    }
    return cache;
  };
}

// The stage0 bytes, either loaded from a snapshot
// (schism-stage0.wasm) or compiled by the host Scheme.
export const stage0_bytes = make_cache(() =>
  OPTIONS.stage0 ? (async function() {
    return OPTIONS.use_snapshot
	? fs.readFileSync(root.join('schism-stage0.wasm'))
	: await compileBootstrap()
  })() : undefined);
// Compile bytes using the stage0 compiler.
export const stage0_compile = OPTIONS.stage0 ? make_compiler(stage0_bytes, "Stage0 compiler") : undefined;
export const stage1_bytes = make_cache(async () => {
    if (!OPTIONS.stage1) { return undefined; }
    const bytes = await stage0_compile(fs.readFileSync(compilerPath), "compiler.ss");
    fs.writeFileSync(root.join('schism-stage1.wasm'), bytes);
    return bytes;
});

export const stage1_compile = OPTIONS.stage1 ? make_compiler(stage1_bytes, "Stage1 compiler") : undefined;
export const stage2_bytes = make_cache(async () => {
    if (!OPTIONS.stage2) { return undefined; }
    const bytes = await stage1_compile(fs.readFileSync(compilerPath), "compiler.ss");
    fs.writeFileSync(root.join('schism-stage2.wasm'), bytes);
    return bytes;
});

export const stage2_compile = OPTIONS.stage2 ? make_compiler(stage2_bytes, "Stage2 compiler") : undefined;
export const stage3_bytes = make_cache(async () => {
    if (!OPTIONS.stage3) { return undefined; }
    const bytes = await stage2_compile(fs.readFileSync(compilerPath), "compiler.ss");
    fs.writeFileSync(root.join('schism-stage3.wasm'), bytes);
    return bytes;
});
