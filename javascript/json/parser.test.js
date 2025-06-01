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
