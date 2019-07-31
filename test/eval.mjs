
export function test(wasm, engine, assert) {
  engine.setCurrentInputPortChars(`
    (let fib ((n 20))
      (if (< n 2)
          n
          (+ (fib (- n 1)) (fib (- n 2)))))`);

  const result = wasm.call('read-and-eval');

  assert.equal(result, 6765, "fib(20)");
}
