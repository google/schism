
export function test(wasm, engine, assert) {
  assert.equal(wasm.call('do-test', 1, 2), 3,
               "Added the numbers and converted back.");
}
