const { lexer } = require("./lexer");
const { describe, it } = require("node:test");
const assert = require("node:assert");

describe("string", () => {
  it("should tokenize a string literal", () => {
    const input = '"hello"';
    const result = lexer(input);
    assert.deepStrictEqual(result, [{ type: "STRING", value: "hello" }]);
  });

  it("should throw an error for unterminated string literals", () => {
    const input = '"hello';
    assert.throws(() => lexer(input), /Unterminated string literal/);
  });

  it("should return empty array for an empty input", () => {
    const input = "";
    const result = lexer(input);
    assert.deepStrictEqual(result, []);
  });

  it("should ignore whitespace before a string", () => {
    const input = '   "hello"';
    const result = lexer(input);
    assert.deepStrictEqual(result, [{ type: "STRING", value: "hello" }]);
  });

  it("should throw an error if no valid token is found (unquoted string)", () => {
    const input = "@";
    assert.throws(() => lexer(input));
  });
});

describe("number", () => {
  it("should tokenize a number literal", () => {
    const input = "123";
    const result = lexer(input);
    assert.deepStrictEqual(result, [{ type: "NUMBER", value: 123 }]);
  });

  it("should throw an error for invalid number literals", () => {
    const input = "123abc";
    assert.throws(() => lexer(input));
  });
});

describe("boolean", () => {
  it("should tokenize a boolean literal (true)", () => {
    const input = "true";
    const result = lexer(input);
    assert.deepStrictEqual(result, [{ type: "BOOLEAN", value: true }]);
  });

  it("should tokenize a boolean literal (false)", () => {
    const input = "false";
    const result = lexer(input);
    assert.deepStrictEqual(result, [{ type: "BOOLEAN", value: false }]);
  });

  it("should throw an error for invalid boolean literals", () => {
    const input = "true@";
    assert.throws(() => lexer(input));
  });

  it("should throw an error for invalid boolean literals", () => {
    const input = "false@";
    assert.throws(() => lexer(input));
  });

  it("should throw an error for invalid boolean literals", () => {
    const input = "true123";
    assert.throws(() => lexer(input));
  });

  it("should throw an error for invalid boolean literals", () => {
    const input = "false123";
    assert.throws(() => lexer(input));
  });

  it("should throw an error for invalid boolean literals", () => {
    const input = "truetrue";
    assert.throws(() => lexer(input));
  });

  it("should throw an error for invalid boolean literals", () => {
    const input = "falsetrue";
    assert.throws(() => lexer(input));
  });
});

describe("null", () => {
  it("should tokenize a null literal", () => {
    const input = "null";
    const result = lexer(input);
    assert.deepStrictEqual(result, [{ type: "NULL", value: null }]);
  });

  it("should throw an error for invalid null literals", () => {
    const input = "null@";
    assert.throws(() => lexer(input));
  });

  it("should throw an error for invalid null literals", () => {
    const input = "null123";
    assert.throws(() => lexer(input));
  });

  it("should throw an error for invalid null literals", () => {
    const input = "nullnull";
    assert.throws(() => lexer(input));
  });
});
