// -*- javascript -*-

import { stage2_bytes } from './run-utils';

import fs from 'fs';

console.info("Compiling stage2 compiler");
stage2_bytes.then((bytes) => {
  console.info("Saving current stage2 compiler as new stage0 snapshot");
  fs.writeFileSync('schism-stage0.wasm', bytes);
})
