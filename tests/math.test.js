import test from "node:test";
import assert from "node:assert/strict";

// Pure JS reference implementation of math.asl algorithms
function aslSqrt(x) {
  if (x <= 0) return 0;
  let g = x / 2;
  for (let i = 0; i < 10; i++) {
    g = (g + x / g) / 2;
  }
  return g;
}

function aslDot(a, b) {
  let sum = 0;
  for (let i = 0; i < a.length; i++) sum += a[i] * b[i];
  return sum;
}

function aslNorm(v) {
  return aslSqrt(aslDot(v, v));
}

function aslCosineSim(a, b) {
  const na = aslNorm(a);
  const nb = aslNorm(b);
  if (na <= 0 || nb <= 0) return 0;
  return aslDot(a, b) / (na * nb);
}

function aslRelu(v) {
  return v.map(x => x > 0 ? x : 0);
}

function aslSoftmax(v) {
  const exps = v.map(x => Math.max(1.0 + x + (x * x) / 2.0, 0.0001));
  const sum = exps.reduce((acc, x) => acc + x, 0);
  return exps.map(x => x / sum);
}

test("aslSqrt calculates square root via Newton-Raphson", () => {
  assert(Math.abs(aslSqrt(16.0) - 4.0) < 1e-9);
  assert(Math.abs(aslSqrt(2.0) - Math.SQRT2) < 1e-9);
  assert(aslSqrt(0.0) === 0);
});

test("Vector math: dot product, norm, and cosine similarity", () => {
  const a = [1.0, 2.0, 3.0];
  const b = [4.0, 5.0, 6.0];
  assert.equal(aslDot(a, b), 32.0);

  const c = [3.0, 4.0];
  assert(Math.abs(aslNorm(c) - 5.0) < 1e-9);

  // Orthogonal vectors
  const v1 = [1.0, 0.0];
  const v2 = [0.0, 1.0];
  assert(Math.abs(aslCosineSim(v1, v2)) < 1e-9);

  // Collinear vectors
  assert(Math.abs(aslCosineSim(v1, v1) - 1.0) < 1e-9);
});

test("Activations: ReLU and Softmax", () => {
  const inputs = [-5.0, 0.0, 3.0, -1.0];
  assert.deepEqual(aslRelu(inputs), [0.0, 0.0, 3.0, 0.0]);

  const sm = aslSoftmax([1.0, 1.0]);
  assert(Math.abs(sm[0] - 0.5) < 1e-6);
  assert(Math.abs(sm[1] - 0.5) < 1e-6);
});
