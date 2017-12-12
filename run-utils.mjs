// -*- javascript -*-
import * as Schism from './rt/rt';

import fs from 'fs';

export const OPTIONS = {
    use_snapshot: true, // load schism-stage0.wasm if true instead of
                         // building with host scheme

    // which stages to build and run tests for
    stage0: true,
    stage1: true,
    stage2: false,
    stage3: false, // compile-only, no point running tests
}

function make_compiler(compiler_bytes) {
    return async function(bytes) {
	const engine = new Schism.Engine;
	const schism = await engine.loadWasmModule(await compiler_bytes);
	engine.setCurrentInputPort(bytes);
	schism.exports['compile-stdin->stdout']();
	return new Uint8Array(engine.output_data);
    }
}

// The stage0 bytes, either loaded from a snapshot
// (schism-stage0.wasm) or compiled by the host Scheme.
export const stage0_bytes = OPTIONS.stage0 ? (async function() {
    return OPTIONS.use_snapshot
	? fs.readFileSync('schism-stage0.wasm')
	: await compileBootstrap()
})() : undefined;
// Compile bytes using the stage0 compiler.
export const stage0_compile = OPTIONS.stage0 ? make_compiler(stage0_bytes) : undefined;
export const stage1_bytes = OPTIONS.stage1 ? stage0_compile(fs.readFileSync('./schism/compiler.ss'))
                                    : undefined;
stage1_bytes.then((bytes) => {
    fs.writeFileSync('schism-stage1.wasm', bytes);
});
export const stage1_compile = OPTIONS.stage1 ? make_compiler(stage1_bytes) : undefined;
export const stage2_bytes = OPTIONS.stage2 ? stage1_compile(fs.readFileSync('./schism/compiler.ss'))
                                    : undefined;
export const stage2_compile = OPTIONS.stage2 ? make_compiler(stage2_bytes) : undefined;
export const stage3_bytes = OPTIONS.stage3 ? stage2_compile(fs.readFileSync('./schism/compiler.ss'))
                                    : undefined;
