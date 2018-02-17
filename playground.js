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

import { rt, js_from_scheme, set_current_input_port, output_data } from "./rt/rt.js";

//const old_peek = rt['peek-char'];
//rt['peek-char'] = function() {
//  const result = old_peek();
//  console.info(`peek: ${result}`);
//  return result;
//}

async function compileSchism() {
  let bytes = (await fetch('schism-stage0.wasm', { credentials: 'include' })).arrayBuffer();
  let result = await WebAssembly.instantiate(await bytes, { 'rt': rt });
  console.info("Loading Schism Complete");
  return result.instance;
}

const compiler = compileSchism();

async function compileAndRun() {
  const src = document.getElementById('src').value;
  console.info(`Compiling program: '${src}'`);
  const schism = await compiler;
  const compile = schism.exports['compile-stdin->stdout'];

  let new_src = [];
  for (let c of src) {
    new_src.push(c.charCodeAt(0));
  }

  set_current_input_port(new_src);
  output_data.length = 0;
  compile();

  console.info("Compilation complete, executing program");

  const bytes = new Uint8Array(output_data);
  const result = (await WebAssembly.instantiate(bytes, { 'rt': rt }));
  set_current_input_port('');
  output_data.length = 0;

  const scheme_result = js_from_scheme(result.instance.exports.main());
  document.getElementById('result').innerHTML = "" + scheme_result;

  console.info("Done");
}

document.getElementById('go').addEventListener('click', () => {
  compileAndRun();
});
