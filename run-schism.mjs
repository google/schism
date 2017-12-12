// -*- javascript -*-
import * as Schism from './rt/rt';
import { stage0_compile, stage1_compile } from './run-utils.mjs';

import fs from 'fs';
import util from 'util';

async function runSchism() {
    // set up the input port
    //const input_file = "./schism/compiler.ss";
    const input_file = "./test/add-num.ss";
    const compiler_output = await stage1_compile(fs.readFileSync(input_file));
    fs.writeFileSync('out.wasm', compiler_output);
}

runSchism().catch((e) => {
    console.error(e.stack);
    throw e;
})
