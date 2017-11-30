// -*- javascript -*-
import { rt, js_from_scheme, set_current_input_port, output_data } from './rt/rt';

import fs from 'fs';
import util from 'util';

async function runSchism() {
  const file = fs.readFileSync('out.wasm');
  const wasm = new WebAssembly.Module(file);

  // set up the input port
  const input_file = "./schism/compiler.ss";
  if (fs.existsSync(input_file)) {
	set_current_input_port(fs.readFileSync(input_file));
  } else {
	set_current_input_port([]);
  }

  const instance = await WebAssembly.instantiate(wasm, { 'rt': rt });

  js_from_scheme(instance.exports['compile-stdin->stdout']());

  //console.info(output_data);
}

runSchism().catch((e) => {
  console.error(e.stack);
  throw e;
})
