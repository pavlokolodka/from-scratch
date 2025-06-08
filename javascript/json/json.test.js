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

      describe("number", () => {
        it("should correctly parse a simple number", () => {
          const jsonString = "42";
          const result = parse(jsonString);
          assert.strictEqual(result, 42);
        });

        it("should correctly parse a negative number", () => {
          const jsonString = "-42";
          const result = parse(jsonString);
          assert.strictEqual(result, -42);
        });

        it("should correctly parse a floating-point number", () => {
          const jsonString = "3.14";
          const result = parse(jsonString);
          assert.strictEqual(result, 3.14);
        });

        it("should correctly parse a number in scientific notation", () => {
          const jsonString = "1.23e4";
          const result = parse(jsonString);
          assert.strictEqual(result, 12300);
        });
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

      it("should throw an error for number with more that one dot", () => {
        const jsonString = "0.4.2";
        assert.throws(() => {
          console.log(parse(jsonString));
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

    it('should stringify Infinity as "null"', () => {
      const input = Infinity;
      const result = stringify(input);
      assert.strictEqual(result, "null");
    });

    it('should stringify NaN as "null"', () => {
      const input = NaN;
      const result = stringify(input);
      assert.strictEqual(result, "null");
    });

    it("should throw an error for BigInt", () => {
      const input = BigInt(123);
      assert.throws(() => {
        stringify(input);
      }, /Cannot serialize BigInt value/);
    });

    it("should return undefined for functions", () => {
      const input = function () {};
      const result = stringify(input);
      assert.strictEqual(result, undefined);
    });

    it("should return undefined for symbols", () => {
      const input = Symbol("test");
      const result = stringify(input);
      assert.strictEqual(result, undefined);
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

    it("should replace undefined with null", () => {
      const input = [1, undefined, "test"];
      const result = stringify(input);
      assert.strictEqual(result, '[1,null,"test"]');
    });

    it("should replace functions with null", () => {
      const input = [1, function () {}, "test"];
      const result = stringify(input);
      assert.strictEqual(result, '[1,null,"test"]');
    });

    it("should replace symbols with null", () => {
      const sym = Symbol("test");
      const input = [1, sym, "test"];
      const result = stringify(input);
      assert.strictEqual(result, '[1,null,"test"]');
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

    it("should ignore undefined", () => {
      const input = { a: 1, b: undefined, c: "test" };
      const result = stringify(input);
      assert.strictEqual(result, '{"a":1,"c":"test"}');
    });

    it("should ignore functions", () => {
      const input = { a: 1, b: function () {}, c: "test" };
      const result = stringify(input);
      assert.strictEqual(result, '{"a":1,"c":"test"}');
    });

    it("should ignore symbols", () => {
      const sym = Symbol("test");
      const input = { a: 1, b: sym, c: "test" };
      const result = stringify(input);
      assert.strictEqual(result, '{"a":1,"c":"test"}');
    });

    it("should return an empty object for Set", () => {
      const input = new Set([1, 2, 3]);
      const result = stringify(input);
      assert.deepStrictEqual(result, "{}");
    });

    it("should return an empty object for Map", () => {
      const input = new Map([
        ["key1", "value1"],
        ["key2", "value2"],
      ]);
      const result = stringify(input);
      assert.deepStrictEqual(result, "{}");
    });

    it("should return an empty object for WeakMap", () => {
      const input = new WeakMap();
      const key1 = {};
      const key2 = {};
      input.set(key1, "value1");
      input.set(key2, "value2");
      const result = stringify(input);
      assert.deepStrictEqual(result, "{}");
    });

    it("should return an empty object for WeakSet", () => {
      const input = new WeakSet();
      const obj1 = {};
      const obj2 = {};
      input.add(obj1);
      input.add(obj2);
      const result = stringify(input);
      assert.deepStrictEqual(result, "{}");
    });

    it("should return an empty object for object with no enumerable properties", () => {
      const input = Object.create(null);
      Object.defineProperty(input, "key", {
        value: 42,
        enumerable: false,
      });
      const result = stringify(input);
      assert.strictEqual(result, "{}");
    });

    it("should stringify a Date object", () => {
      const input = new Date("2023-10-01T12:00:00Z");
      const result = stringify(input);
      assert.strictEqual(result, '"2023-10-01T12:00:00.000Z"');
    });

    it("should stringify a Date object (property)", () => {
      const date = new Date("2023-10-01T12:00:00Z");
      const input = { date };
      const result = stringify(input);
      assert.strictEqual(result, `{"date":"2023-10-01T12:00:00.000Z"}`);
    });
  });
});
