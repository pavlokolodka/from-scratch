const { parse } = require("./parser");
const { describe, it } = require("node:test");
const assert = require("node:assert");

it("should throw an error for an unexpected token type", () => {
  const tokens = [{ type: "INVALID", value: "oops" }];
  assert.throws(() => parse(tokens), /Unexpected token type: INVALID/);
});

it("should throw an error for an empty token array", () => {
  const tokens = [];
  assert.throws(() => parse(tokens));
});

describe("primitive", () => {
  it("should parse a number literal", () => {
    const tokens = [{ type: "NUMBER", value: 123 }];
    const result = parse(tokens);
    assert.strictEqual(result, 123);
  });

  it("should parse a string literal", () => {
    const tokens = [{ type: "STRING", value: "hello" }];
    const result = parse(tokens);
    assert.strictEqual(result, "hello");
  });

  it("should parse a boolean literal (true)", () => {
    const tokens = [{ type: "BOOLEAN", value: true }];
    const result = parse(tokens);
    assert.strictEqual(result, true);
  });

  it("should parse a boolean literal (false)", () => {
    const tokens = [{ type: "BOOLEAN", value: false }];
    const result = parse(tokens);
    assert.strictEqual(result, false);
  });

  it("should parse a null literal", () => {
    const tokens = [{ type: "NULL", value: null }];
    const result = parse(tokens);
    assert.strictEqual(result, null);
  });

  it("should parse the first valid token and ignore the rest", () => {
    const tokens = [
      { type: "STRING", value: "hello" },
      { type: "NUMBER", value: 123 },
    ];
    const result = parse(tokens);
    assert.strictEqual(result, "hello");
  });
});

describe("object", () => {
  it("should parse an empty object", () => {
    const tokens = [
      { type: "OPEN_OBJECT", value: "{" },
      { type: "CLOSE_OBJECT", value: "}" },
    ];
    const result = parse(tokens);
    assert.deepStrictEqual(result, {});
  });

  it("should parse an object with one key-value pair", () => {
    const tokens = [
      { type: "OPEN_OBJECT", value: "{" },
      { type: "STRING", value: "key" },
      { type: "COLON", value: ":" },
      { type: "STRING", value: "value" },
      { type: "CLOSE_OBJECT", value: "}" },
    ];
    const result = parse(tokens);
    assert.deepStrictEqual(result, { key: "value" });
  });

  it("should parse an object with multiple key-value pairs", () => {
    const tokens = [
      { type: "OPEN_OBJECT", value: "{" },
      { type: "STRING", value: "key1" },
      { type: "COLON", value: ":" },
      { type: "STRING", value: "value1" },
      { type: "COMMA", value: "," },
      { type: "STRING", value: "key2" },
      { type: "COLON", value: ":" },
      { type: "NUMBER", value: 42 },
      { type: "CLOSE_OBJECT", value: "}" },
    ];
    const result = parse(tokens);
    assert.deepStrictEqual(result, { key1: "value1", key2: 42 });
  });

  it("should parse an object with a nested object", () => {
    const tokens = [
      { type: "OPEN_OBJECT", value: "{" },
      { type: "STRING", value: "outerKey" },
      { type: "COLON", value: ":" },
      { type: "OPEN_OBJECT", value: "{" },
      { type: "STRING", value: "innerKey" },
      { type: "COLON", value: ":" },
      { type: "STRING", value: "innerValue" },
      { type: "CLOSE_OBJECT", value: "}" },
      { type: "COMMA", value: "," },
      { type: "STRING", value: "key" },
      { type: "COLON", value: ":" },
      { type: "NUMBER", value: 123 },
      { type: "CLOSE_OBJECT", value: "}" },
    ];
    const result = parse(tokens);
    assert.deepStrictEqual(result, {
      outerKey: { innerKey: "innerValue" },
      key: 123,
    });
  });
});
