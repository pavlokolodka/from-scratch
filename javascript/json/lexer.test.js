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

  it("should return empty string for an empty string input", () => {
    const input = '""';
    const result = lexer(input);
    assert.deepStrictEqual(result, [{ type: "STRING", value: "" }]);
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

describe("object", () => {
  it("should tokenize an empty object", () => {
    const input = "{}";
    const result = lexer(input);
    assert.deepStrictEqual(result, [
      { type: "OPEN_OBJECT", value: "{" },
      { type: "CLOSE_OBJECT", value: "}" },
    ]);
  });

  it("should tokenize an object with a single key-value pair", () => {
    const input = '{"key": "value"}';
    const result = lexer(input);
    assert.deepStrictEqual(result, [
      { type: "OPEN_OBJECT", value: "{" },
      { type: "STRING", value: "key" },
      { type: "COLON", value: ":" },
      { type: "STRING", value: "value" },
      { type: "CLOSE_OBJECT", value: "}" },
    ]);
  });

  it("should throw an error for unterminated objects", () => {
    const input = '{"key": "value",}';
    assert.throws(() => lexer(input));
  });

  it("should throw an error for trailing comma", () => {
    const input = '["element",]';
    assert.throws(() => lexer(input));
  });
});

describe("array", () => {
  it("should tokenize an empty array", () => {
    const input = "[]";
    const result = lexer(input);
    assert.deepStrictEqual(result, [
      { type: "OPEN_ARRAY", value: "[" },
      { type: "CLOSE_ARRAY", value: "]" },
    ]);
  });

  it("should tokenize an array with a single element", () => {
    const input = '["element"]';
    const result = lexer(input);
    assert.deepStrictEqual(result, [
      { type: "OPEN_ARRAY", value: "[" },
      { type: "STRING", value: "element" },
      { type: "CLOSE_ARRAY", value: "]" },
    ]);
  });

  it("should throw an error for unterminated arrays", () => {
    const input = '["element"';
    assert.throws(() => lexer(input));
  });

  it("should throw an error for trailing comma", () => {
    const input = '["element",]';
    assert.throws(() => lexer(input));
  });
});
