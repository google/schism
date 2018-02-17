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

import * as Schism from './rt/rt';
import { stage0_compile, stage1_compile } from './run-utils.mjs';

import fs from 'fs';
import util from 'util';

async function runSchism() {
    // set up the input port
    const input_file = "./schism/compiler.ss";
    //const input_file = "./test/add-num.ss";
    const compiler_output = await stage0_compile(fs.readFileSync(input_file));
    fs.writeFileSync('out.wasm', compiler_output);
}

runSchism().catch((e) => {
    console.error(e.stack);
    throw e;
})
