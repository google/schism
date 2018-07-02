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
import { stage0_compile, stage1_compile, stage2_compile } from './run-utils.mjs';

import fs from 'fs';
import util from 'util';
import process from 'process';
import cli from './cli';

async function runSchism() {
    // set up the input port
    const input_file = cli.input[0] || "./schism/compiler.ss";
    const compiler_output = await getCompile()(fs.readFileSync(input_file));
    fs.writeFileSync(cli.flags.out, compiler_output);
}

function getCompile() {
    switch(cli.flags.stage) {
        case "1":
            return stage1_compile;
        case "2":
            return stage2_compile;
        case "0":
        default:
            return stage0_compile;
    }
}

runSchism().catch((e) => {
    console.error(e.stack);
    throw e;
})
