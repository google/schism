
export function test(wasm, engine, assert) {
  function testValue(value) {
    let rawResult = wasm.exports['do-test'](
      engine.schemeFromJs(value)
    );
    return engine.jsFromScheme(rawResult);
  }

  assert.equal(testValue(false), true);
  assert.equal(testValue(true), false);
}

