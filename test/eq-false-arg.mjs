
export function test(wasm, engine, assert) {
  assert.equal(wasm.call('do-test', false), true);
  assert.equal(wasm.call('do-test', true), false);
}

