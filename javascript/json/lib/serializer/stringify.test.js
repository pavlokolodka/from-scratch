const { stringify } = require("./stringify.js");
const { describe, it } = require("node:test");
const assert = require("node:assert");

describe("primitives", () => {
  it("should stringify a number", () => {
    const input = 123;
    const result = stringify(input);
    assert.strictEqual(result, "123");
  });

  it("should stringify a string", () => {
    const input = "hello";
    const result = stringify(input);
    assert.strictEqual(result, '"hello"');
  });

  it("should stringify a boolean (true)", () => {
    const input = true;
    const result = stringify(input);
    assert.strictEqual(result, "true");
  });

  it("should stringify a boolean (false)", () => {
    const input = false;
    const result = stringify(input);
    assert.strictEqual(result, "false");
  });

  it("should stringify null", () => {
    const input = null;
    const result = stringify(input);
    assert.strictEqual(result, "null");
  });
});

describe("arrays", () => {
  it("should stringify an empty array", () => {
    const input = [];
    const result = stringify(input);
    assert.strictEqual(result, "[]");
  });

  it("should stringify a non-empty array", () => {
    const input = [1, "two", false];
    const result = stringify(input);
    assert.strictEqual(result, '[1,"two",false]');
  });

  it("should stringify a nested array", () => {
    const input = [1, ["two", false]];
    const result = stringify(input);
    assert.strictEqual(result, '[1,["two",false]]');
  });

  it("should stringify an array with null values", () => {
    const input = [null, null];
    const result = stringify(input);
    assert.strictEqual(result, "[null,null]");
  });
});

describe("objects", () => {
  it("should stringify an empty object", () => {
    const input = {};
    const result = stringify(input);
    assert.strictEqual(result, "{}");
  });

  it("should stringify a non-empty object", () => {
    const input = { a: 1, b: "two", c: false };
    const result = stringify(input);
    assert.strictEqual(result, '{"a":1,"b":"two","c":false}');
  });

  it("should stringify a nested object", () => {
    const input = { a: 1, b: { c: "three", d: false } };
    const result = stringify(input);
    assert.strictEqual(result, '{"a":1,"b":{"c":"three","d":false}}');
  });

  it("should stringify an object with null values", () => {
    const input = { a: null, b: null };
    const result = stringify(input);
    assert.strictEqual(result, '{"a":null,"b":null}');
  });
});
