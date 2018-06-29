
export function test(wasm, engine, assert) {
  let rawResult = wasm.exports['do-test'](
    engine.schemeFromJs(1),
    engine.schemeFromJs(2)
  );
  let result = engine.jsFromScheme(rawResult);

  assert.equal(result, 3, "Added the numbers and converted back.");
}
