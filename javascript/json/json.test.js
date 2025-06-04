const { parse, stringify } = require("./json");
const { describe, it } = require("node:test");
const assert = require("node:assert");

describe("parse", () => {
  describe("primitives", () => {
    describe("should parse", () => {
      it("should correctly parse a simple string", () => {
        const jsonString = '"Hello, World!"';
        const result = parse(jsonString);
        assert.strictEqual(result, "Hello, World!");
      });

      it("should correctly parse a simple number", () => {
        const jsonString = "42";
        const result = parse(jsonString);
        assert.strictEqual(result, 42);
      });

      it("should correctly parse null", () => {
        const jsonString = "null";
        const result = parse(jsonString);
        assert.strictEqual(result, null);
      });
    });

    describe("should throw an error", () => {
      it("should throw an error for an unclosed string", () => {
        const jsonString = '"Hello, World!';
        assert.throws(() => {
          parse(jsonString);
        });
      });

      it("should throw an error for an invalid number with a trailing comma", () => {
        const jsonString = "42,";
        assert.throws(() => {
          parse(jsonString);
        });
      });

      it("should throw an error for a malformed null value", () => {
        const jsonString = "nll";
        assert.throws(() => {
          parse(jsonString);
        });
      });

      it("should throw an error for a number with extra characters", () => {
        const jsonString = "42abc";
        assert.throws(() => {
          parse(jsonString);
        });
      });
    });
  });

  describe("object", () => {
    describe("should parse", () => {
      it("should correctly parse a simple JSON object", () => {
        const jsonString = '{"name": "Alice", "age": 25}';
        const result = parse(jsonString);
        assert.deepStrictEqual(result, { name: "Alice", age: 25 });
      });

      it("should correctly parse an empty JSON object", () => {
        const jsonString = "{}";
        const result = parse(jsonString);
        assert.deepStrictEqual(result, {});
      });

      it("should correctly parse JSON with boolean values", () => {
        const jsonString = '{"isActive": true, "isVerified": false}';
        const result = parse(jsonString);
        assert.deepStrictEqual(result, { isActive: true, isVerified: false });
      });

      it("should correctly parse a JSON with null", () => {
        const jsonString = '{"value": null}';
        const result = parse(jsonString);
        assert.deepStrictEqual(result, { value: null });
      });

      it("should correctly parse a JSON string with a number", () => {
        const jsonString = '{"age": 30}';
        const result = parse(jsonString);
        assert.deepStrictEqual(result, { age: 30 });
      });

      it("should correctly parse nested JSON objects", () => {
        const jsonString =
          '{"person": {"name": "Bob", "age": 30}, "active": true}';
        const result = parse(jsonString);
        assert.deepStrictEqual(result, {
          person: { name: "Bob", age: 30 },
          active: true,
        });
      });
    });

    describe("should throw an error", () => {
      it("should throw an error for non-JSON input", () => {
        const jsonString = "This is not JSON";
        assert.throws(() => {
          parse(jsonString);
        });
      });

      it("should throw an error for invalid JSON (unexpected token)", () => {
        const jsonString = '{"name": "Alice", "age": 25';
        assert.throws(() => {
          parse(jsonString);
        });
      });

      it("should throw an error for invalid JSON (extra commas)", () => {
        const jsonString = '{"name": "Alice", "age": 25,}';
        assert.throws(() => {
          parse(jsonString);
        });
      });
    });
  });

  describe("array", () => {
    describe("should parse", () => {
      it("should correctly parse a simple JSON array", () => {
        const jsonString = "[1, 2, 3, 4]";
        const result = parse(jsonString);
        assert.deepStrictEqual(result, [1, 2, 3, 4]);
      });

      it("should correctly parse a nested JSON array", () => {
        const jsonString = "[1, [2, 3], 4]";
        const result = parse(jsonString);
        assert.deepStrictEqual(result, [1, [2, 3], 4]);
      });

      it("should correctly parse a JSON array with mixed types", () => {
        const jsonString = '[1, "two", true, null, { "key": "value" }]';
        const result = parse(jsonString);
        assert.deepStrictEqual(result, [
          1,
          "two",
          true,
          null,
          { key: "value" },
        ]);
      });

      it("should correctly parse a deeply nested JSON array", () => {
        const jsonString = "[1, [2, [3, [4, 5]]]]";
        const result = parse(jsonString);
        assert.deepStrictEqual(result, [1, [2, [3, [4, 5]]]]);
      });
    });

    describe("should throw an error", () => {
      it("should throw an error for array with trailing comma", () => {
        const jsonString = "[1, 2, 3, 4,]";
        assert.throws(() => {
          parse(jsonString);
        });
      });

      it("should throw an error for array with missing closing bracket", () => {
        const jsonString = "[1, 2, 3";
        assert.throws(() => {
          parse(jsonString);
        });
      });

      it("should throw an error for array with extra comma", () => {
        const jsonString = "[1,, 2]";
        assert.throws(() => {
          parse(jsonString);
        });
      });

      it("should throw an error for array with invalid syntax (missing value between commas)", () => {
        const jsonString = "[1, , 2]";
        assert.throws(() => {
          parse(jsonString);
        });
      });
    });
  });
});

describe("stringify", () => {
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
});
