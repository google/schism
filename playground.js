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

import * as Schism from  './rt/rt.js';

async function compileSchism() {
  const schism_bytes = await fetch('schism-stage0.wasm', { credentials: 'include' });
  const engine = new Schism.Engine;
  const schism = await engine.loadWasmModule(await schism_bytes.arrayBuffer());
  console.info('Loading Schism Complete');
  return { schism, engine };
}

const compiler = compileSchism();

async function compileAndRun() {
  const src = document.getElementById('src').value;
  console.info(`Compiling program: '${src}'`);
  const { schism, engine } = await compiler;

  engine.setCurrentInputPortChars(src);
  engine.output_data.length = 0;
  schism.call('compile-stdin->stdout');
  const bytes = new Uint8Array(engine.output_data);
  engine.output_data.length = 0;

  console.info("Compilation complete, executing program");

  const mod = await engine.loadWasmModule(bytes);

  engine.setCurrentInputPort('');
  const result = mod.call('main');

  document.getElementById('result').innerHTML = "" + result;

  console.info("Done");
}

document.getElementById('go').addEventListener('click', () => {
  compileAndRun();
});
