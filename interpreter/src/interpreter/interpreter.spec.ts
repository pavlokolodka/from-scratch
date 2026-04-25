import type { FunctionValue } from './values/function.value';
import type { StringValue } from './values/string.value';
import { Lexer } from '../lexer/lexer';
import { Parser } from '../parser/parser';
import { Environment } from './environment';
import { Interpreter } from './interpreter';
import { RuntimeType } from './interpreter.interface';

describe('Interpreter', () => {
  let interpreter: Interpreter;

  beforeEach(() => {
    interpreter = new Interpreter();
  });

  function evaluate(input: string) {
    const lexer = new Lexer(input);
    const parser = new Parser(lexer.tokenize());
    const env = new Environment();
    const program = parser.parse();

    const stmt = program.statements[0];

    return interpreter.eval(stmt, env);
  }

  function evaluateAll(input: string) {
    const lexer = new Lexer(input);
    const parser = new Parser(lexer.tokenize());
    const env = new Environment();
    const program = parser.parse();

    let result: ReturnType<typeof interpreter.eval>;
    for (const stmt of program.statements) {
      result = interpreter.eval(stmt, env);
    }
    return result!;
  }

  describe('number literal', () => {
    it.each([
      { input: '5', expected: 5 },
      { input: '10', expected: 10 },
    ])('should evaluate number literal $input to $expected', ({ input, expected }) => {
      const result = evaluate(input);
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
    });

    describe('number operations', () => {
      it.each([
        { input: '5 + 5', expected: 10 },
        { input: '5 - 5', expected: 0 },
        { input: '5 * 5', expected: 25 },
        { input: '5 / 5', expected: 1 },
        { input: '5 + 5 + 5 + 5 - 10', expected: 10 },
        { input: '2 * 2 * 2 * 2 * 2', expected: 32 },
        { input: '50 / 2 * 2 + 10', expected: 60 },
        { input: '2 + 3 * 4', expected: 14 },
        { input: '3 * 4 + 2', expected: 14 },
        { input: '2 * (3 + 4)', expected: 14 },
        { input: '(2 * (3 + 4))', expected: 14 },
        { input: '((5 + 5) * 5) * 5', expected: 250 },
      ])('should evaluate infix expression $input to $expected', ({ input, expected }) => {
        const result = evaluate(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
      });

      it.each([
        { input: '1 < 2', expected: true },
        { input: '2 < 1', expected: false },
        { input: '1 < 1', expected: false },
        { input: '2 > 1', expected: true },
        { input: '1 > 2', expected: false },
        { input: '1 > 1', expected: false },
        { input: '1 <= 2', expected: true },
        { input: '2 <= 1', expected: false },
        { input: '1 <= 1', expected: true },
        { input: '2 >= 1', expected: true },
        { input: '1 >= 2', expected: false },
        { input: '1 >= 1', expected: true },
        { input: '1 == 1', expected: true },
        { input: '1 == 2', expected: false },
        { input: '1 != 1', expected: false },
        { input: '1 != 2', expected: true },
        { input: 'true == true', expected: true },
        { input: 'true == false', expected: false },
        { input: 'false != true', expected: true },
        { input: '"a" == "a"', expected: true },
        { input: '"a" == "b"', expected: false },
        { input: 'nil == nil', expected: true },
        { input: 'nil != nil', expected: false },
        { input: '1 == "1"', expected: false },
      ])('should evaluate comparison $input to $expected', ({ input, expected }) => {
        const result = evaluate(input);
        expect(result).toEqual({ type: RuntimeType.BOOLEAN, value: expected });
      });

      it.each([
        { input: '(5' },
        { input: '(5 + 5' },
        { input: '((5 + 5)' },
        { input: '5 + (5 * 5' },
        { input: '(2 * (3 + 4' },
      ])('should throw error for missing closing parenthesis in $input', ({ input }) => {
        expect(() => evaluate(input)).toThrow('Closing parentheses not found');
      });

      describe('-', () => {
        it.each([
          { input: '-5', expected: -5 },
          { input: '-10', expected: -10 },
          { input: '--5', expected: 5 },
        ])('should evaluate $input to $expected', ({ input, expected }) => {
          expect(evaluate(input)).toEqual({ type: RuntimeType.NUMBER, value: expected });
        });

        it('should throw for non-number type', () => {
          expect(() => evaluate('-"a"')).toThrow("Operator '-' not supported for type STRING");
        });
      });
    });
  });

  describe('string literals', () => {
    it.each([
      { input: '"hello"', expected: 'hello' },
      { input: '"world"', expected: 'world' },
      { input: '""', expected: '' },
      { input: '"hello world"', expected: 'hello world' },
    ])('should evaluate string literal $input to $expected', ({ input, expected }) => {
      const result = evaluate(input) as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: expected });
    });

    it('should store a string in a let variable', () => {
      const result = evaluateAll('let s = "hello"; s') as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'hello' });
    });

    it('should store a string in a const variable', () => {
      const result = evaluateAll('const greeting = "hi"; greeting') as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'hi' });
    });

    it('should reassign a string variable', () => {
      const result = evaluateAll('let s = "hello"; s = "world"; s') as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'world' });
    });

    describe('assignments with prefix expressions', () => {
      it.each([
        { input: 'let x = 5; x = -x; x', expected: -5, type: RuntimeType.NUMBER },
        { input: 'let x = 5; x = -(-x); x', expected: 5, type: RuntimeType.NUMBER },
        { input: 'let x = true; x = !x; x', expected: false, type: RuntimeType.BOOLEAN },
        { input: 'let x = false; x = !!x; x', expected: false, type: RuntimeType.BOOLEAN },
        { input: 'let x = 10; x = !x; x', expected: false, type: RuntimeType.BOOLEAN },
        { input: 'let x = 0; x = !x; x', expected: true, type: RuntimeType.BOOLEAN },
        { input: 'let x = "hello"; x = !x; x', expected: false, type: RuntimeType.BOOLEAN },
        { input: 'let x = ""; x = !x; x', expected: true, type: RuntimeType.BOOLEAN },
        { input: 'let x = nil; x = !x; x', expected: true, type: RuntimeType.BOOLEAN },
        { input: 'let x = [1]; x = !x; x', expected: false, type: RuntimeType.BOOLEAN },
        { input: 'let x = []; x = !x; x', expected: true, type: RuntimeType.BOOLEAN },
        { input: 'let x = [5]; let y = -x[0]; y', expected: -5, type: RuntimeType.NUMBER },
        { input: 'let x = [true]; let y = !x[0]; y', expected: false, type: RuntimeType.BOOLEAN },
        { input: 'let x = [false]; let y = !!x[0]; y', expected: false, type: RuntimeType.BOOLEAN },
      ])('should evaluate assignment with prefix $input to $expected', ({
        input,
        expected,
        type,
      }) => {
        expect(evaluateAll(input)).toEqual({ type, value: expected });
      });
    });

    it('should pass a string as a function argument', () => {
      const input = 'fn identity(x) { return x }; identity("hello")';
      const result = evaluateAll(input) as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'hello' });
    });

    it('should return a string from a function', () => {
      const input = 'fn greet() { return "hello" }; greet()';
      const result = evaluateAll(input) as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'hello' });
    });

    describe('string operations', () => {
      it.each([
        { input: '"hello" + "world"', desc: 'string + string' },
        { input: '"hello" - "world"', desc: 'string - string' },
        { input: '"hello" * "world"', desc: 'string * string' },
        { input: '"hello" / "world"', desc: 'string / string' },
        { input: '"hello" + 1', desc: 'string + number' },
        { input: '1 + "hello"', desc: 'number + string' },
        { input: '"hello" * 2', desc: 'string * number' },
      ])('should throw for $desc', ({ input }) => {
        expect(() => evaluate(input)).toThrow();
      });
    });
  });

  describe('boolean literals', () => {
    it.each([
      { input: 'true', expected: true },
      { input: 'false', expected: false },
    ])('should evaluate $input to $expected', ({ input, expected }) => {
      expect(evaluate(input)).toEqual({ type: RuntimeType.BOOLEAN, value: expected });
    });
  });

  describe('null literal', () => {
    it('should evaluate nil to null', () => {
      expect(evaluate('nil')).toEqual({ type: RuntimeType.NULL, value: null });
    });
  });

  describe('array', () => {
    it('should evaluate an empty array', () => {
      const result = evaluate('[]');
      expect(result).toEqual({ type: RuntimeType.ARRAY, value: [] });
    });

    it.each([
      {
        input: '[1, 2, 3]',
        expected: [
          { type: RuntimeType.NUMBER, value: 1 },
          { type: RuntimeType.NUMBER, value: 2 },
          { type: RuntimeType.NUMBER, value: 3 },
        ],
      },
      {
        input: '["a", "b"]',
        expected: [
          { type: RuntimeType.STRING, value: 'a' },
          { type: RuntimeType.STRING, value: 'b' },
        ],
      },
      {
        input: '[1 + 2, 3 * 4]',
        expected: [
          { type: RuntimeType.NUMBER, value: 3 },
          { type: RuntimeType.NUMBER, value: 12 },
        ],
      },
      {
        input: '[true, false]',
        expected: [
          { type: RuntimeType.BOOLEAN, value: true },
          { type: RuntimeType.BOOLEAN, value: false },
        ],
      },
      {
        input: '[nil, nil]',
        expected: [
          { type: RuntimeType.NULL, value: null },
          { type: RuntimeType.NULL, value: null },
        ],
      },
    ])('should evaluate $input to array of values', ({ input, expected }) => {
      const result = evaluate(input);
      expect(result).toEqual({ type: RuntimeType.ARRAY, value: expected });
    });

    it('should evaluate nested arrays', () => {
      const result = evaluate('[1, [2, 3]]');
      expect(result).toEqual({
        type: RuntimeType.ARRAY,
        value: [
          { type: RuntimeType.NUMBER, value: 1 },
          {
            type: RuntimeType.ARRAY,
            value: [
              { type: RuntimeType.NUMBER, value: 2 },
              { type: RuntimeType.NUMBER, value: 3 },
            ],
          },
        ],
      });
    });

    describe('array index access', () => {
      it.each([
        { input: '[1, 2, 3][0]', expected: { type: RuntimeType.NUMBER, value: 1 } },
        { input: '[1, 2, 3][1]', expected: { type: RuntimeType.NUMBER, value: 2 } },
        { input: '[1, 2, 3][2]', expected: { type: RuntimeType.NUMBER, value: 3 } },
        { input: '["a", "b", "c"][0]', expected: { type: RuntimeType.STRING, value: 'a' } },
        { input: '["a", "b", "c"][2]', expected: { type: RuntimeType.STRING, value: 'c' } },
        { input: '[true, false, true][1]', expected: { type: RuntimeType.BOOLEAN, value: false } },
        { input: '[nil, nil, nil][0]', expected: { type: RuntimeType.NULL, value: null } },
      ])('should evaluate $input to expected value', ({ input, expected }) => {
        expect(evaluate(input)).toEqual(expected);
      });

      it('should evaluate index access on a variable', () => {
        const result = evaluateAll('let arr = [10, 20, 30]; arr[0]');
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 10 });
      });

      it('should evaluate index access with an expression index', () => {
        const result = evaluateAll('let arr = [10, 20, 30]; arr[1 + 1]');
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 30 });
      });

      it('should evaluate index access on a nested array', () => {
        const result = evaluate('[1, [2, 3]][1]');
        expect(result).toEqual({
          type: RuntimeType.ARRAY,
          value: [
            { type: RuntimeType.NUMBER, value: 2 },
            { type: RuntimeType.NUMBER, value: 3 },
          ],
        });
      });

      it('should throw for out-of-bounds index', () => {
        expect(() => evaluate('[1, 2, 3][5]')).toThrow('Index out of bounds: 5');
      });

      it('should throw for negative index', () => {
        expect(() => evaluate('[1, 2][0 - 1]')).toThrow('Index out of bounds: -1');
      });

      it('should throw for non-number index', () => {
        expect(() => evaluateAll('let arr = [1, 2]; arr["a"]')).toThrow(
          'Index must be a number, got STRING',
        );
      });

      it('should throw when indexing a non-array value', () => {
        expect(() => evaluateAll('let x = 5; x[0]')).toThrow(
          'Index operator not supported for type NUMBER',
        );
      });
    });

    describe('array mutation', () => {
      it.each([
        {
          desc: 'number',
          input: 'let arr = [1, 2, 3]; arr[0] = 99; arr[0]',
          expected: { type: RuntimeType.NUMBER, value: 99 },
        },
        {
          desc: 'string',
          input: 'let arr = ["a", "b", "c"]; arr[1] = "z"; arr[1]',
          expected: { type: RuntimeType.STRING, value: 'z' },
        },
        {
          desc: 'array',
          input: 'let arr = [1, 2, 3]; arr[2] = [4, 5]; arr[2]',
          expected: {
            type: RuntimeType.ARRAY,
            value: [
              { type: RuntimeType.NUMBER, value: 4 },
              { type: RuntimeType.NUMBER, value: 5 },
            ],
          },
        },
        {
          desc: 'boolean',
          input: 'let arr = [true, false]; arr[0] = false; arr[0]',
          expected: { type: RuntimeType.BOOLEAN, value: false },
        },
        {
          desc: 'null',
          input: 'let arr = [1, 2]; arr[0] = nil; arr[0]',
          expected: { type: RuntimeType.NULL, value: null },
        },
      ])('should mutate element with $desc value', ({ input, expected }) => {
        expect(evaluateAll(input)).toEqual(expected);
      });

      it('should mutate using an expression index', () => {
        const result = evaluateAll('let arr = [10, 20, 30]; arr[1 + 1] = 99; arr[2]');
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 99 });
      });

      it('should mutate using an expression value', () => {
        const result = evaluateAll('let arr = [1, 2, 3]; arr[1] = arr[0] + arr[2]; arr[1]');
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 4 });
      });

      it('should not affect sibling elements when mutating one', () => {
        evaluateAll('let arr = [1, 2, 3]; arr[1] = 99');
        const first = evaluateAll('let arr = [1, 2, 3]; arr[1] = 99; arr[0]');
        const last = evaluateAll('let arr = [1, 2, 3]; arr[1] = 99; arr[2]');
        expect(first).toEqual({ type: RuntimeType.NUMBER, value: 1 });
        expect(last).toEqual({ type: RuntimeType.NUMBER, value: 3 });
      });

      it('should mutate the same index twice (double mutation)', () => {
        const result = evaluateAll('let arr = [1, 2, 3]; arr[0] = 10; arr[0] = 20; arr[0]');
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 20 });
      });

      it('should reflect mutation when read via index expression in an expression', () => {
        const result = evaluateAll('let arr = [1, 2, 3]; arr[0] = 5; arr[0] + arr[1]');
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 7 });
      });

      it('should throw for out-of-bounds index', () => {
        expect(() => evaluateAll('let arr = [1, 2]; arr[5] = 99')).toThrow(
          'Index out of bounds: 5',
        );
      });

      it('should throw for negative index', () => {
        expect(() => evaluateAll('let arr = [1, 2]; arr[0 - 1] = 99')).toThrow(
          'Index out of bounds: -1',
        );
      });

      it.each([
        { desc: 'number', input: 'let x = 5; x[0] = 99', type: 'NUMBER' },
        { desc: 'string', input: 'let x = "hi"; x[0] = 99', type: 'STRING' },
        { desc: 'boolean', input: 'let x = true; x[0] = 99', type: 'BOOLEAN' },
      ])('should throw when mutating a non-array ($desc)', ({ input, type }) => {
        expect(() => evaluateAll(input)).toThrow(`Index operator not supported for type ${type}`);
      });

      it('should throw for non-number index', () => {
        expect(() => evaluateAll('let arr = [1, 2]; arr["a"] = 99')).toThrow(
          'Index must be a number, got STRING',
        );
      });
    });
  });

  describe('let declaration', () => {
    describe('number', () => {
      it.each([
        { input: 'let a = 5; a', expected: 5 },
        { input: 'let b = 10; b', expected: 10 },
        { input: 'let c = 2 + 3; c', expected: 5 },
        { input: 'let d = 5; let e = 10; d', expected: 5 },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
      });
    });

    describe('string', () => {
      it.each([
        { input: 'let a = "5"; a', expected: '5' },
        { input: 'let b = "10"; b', expected: '10' },
        { input: 'let c = "hello"; c', expected: 'hello' },
        { input: 'let d = "a"; let e = "b"; d', expected: 'a' },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.STRING, value: expected });
      });
    });

    describe('boolean', () => {
      it.each([
        { input: 'let a = true; a', expected: true },
        { input: 'let b = false; b', expected: false },
        { input: 'let c = true; let d = false; c', expected: true },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.BOOLEAN, value: expected });
      });
    });

    describe('null', () => {
      it('should declare and look up nil', () => {
        expect(evaluateAll('let a = nil; a')).toEqual({ type: RuntimeType.NULL, value: null });
      });
    });

    describe('array', () => {
      it.each([
        { input: 'let a = [5]; a', expected: 5 },
        { input: 'let b = [10]; b', expected: 10 },
        { input: 'let c = [2 + 3]; c', expected: 5 },
        { input: 'let d = [5]; let e = [10]; d', expected: 5 },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({
          type: RuntimeType.ARRAY,
          value: [{ type: RuntimeType.NUMBER, value: expected }],
        });
      });

      it.each([
        { input: 'let a = ["5"]; a', expected: '5' },
        { input: 'let b = ["10"]; b', expected: '10' },
        { input: 'let c = ["hello"]; c', expected: 'hello' },
        { input: 'let d = ["a"]; let e = ["b"]; d', expected: 'a' },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({
          type: RuntimeType.ARRAY,
          value: [{ type: RuntimeType.STRING, value: expected }],
        });
      });

      describe('reassign', () => {
        it.each([
          { input: 'let a = 5; a = 10; a', expected: 10, type: RuntimeType.NUMBER },
          { input: 'let a = "5"; a = "10"; a', expected: '10', type: RuntimeType.STRING },
          { input: 'let a = true; a = false; a', expected: false, type: RuntimeType.BOOLEAN },
        ])('should reassign: $input', ({ input, expected, type }) => {
          const result = evaluateAll(input);
          expect(result).toEqual({ type, value: expected });
        });

        it('should reassign array', () => {
          const result = evaluateAll('let a = [""]; a = [10]; a');
          expect(result).toEqual({
            type: RuntimeType.ARRAY,
            value: [{ type: RuntimeType.NUMBER, value: 10 }],
          });
        });
      });
    });
  });

  describe('const declaration', () => {
    describe('number', () => {
      it.each([
        { input: 'const a = 5; a', expected: 5 },
        { input: 'const b = 10; b', expected: 10 },
        { input: 'const c = 2 + 3; c', expected: 5 },
        { input: 'const d = 5; const e = 10; d', expected: 5 },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
      });
    });

    describe('string', () => {
      it.each([
        { input: 'const a = "5"; a', expected: '5' },
        { input: 'const b = "10"; b', expected: '10' },
        { input: 'const c = "hello"; c', expected: 'hello' },
        { input: 'const d = "a"; const e = "b"; d', expected: 'a' },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.STRING, value: expected });
      });
    });

    describe('boolean', () => {
      it.each([
        { input: 'const a = true; a', expected: true },
        { input: 'const b = false; b', expected: false },
        { input: 'const c = true; const d = false; c', expected: true },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.BOOLEAN, value: expected });
      });
    });

    describe('null', () => {
      it('should declare and look up nil', () => {
        expect(evaluateAll('const a = nil; a')).toEqual({ type: RuntimeType.NULL, value: null });
      });
    });

    describe('array', () => {
      it.each([
        { input: 'const a = [5]; a', expected: 5 },
        { input: 'const b = [10]; b', expected: 10 },
        { input: 'const c = [2 + 3]; c', expected: 5 },
        { input: 'const d = [5]; const e = [10]; d', expected: 5 },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({
          type: RuntimeType.ARRAY,
          value: [{ type: RuntimeType.NUMBER, value: expected }],
        });
      });

      it.each([
        { input: 'const a = ["5"]; a', expected: '5' },
        { input: 'const b = ["10"]; b', expected: '10' },
        { input: 'const c = ["hello"]; c', expected: 'hello' },
        { input: 'const d = ["a"]; const e = ["b"]; d', expected: 'a' },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({
          type: RuntimeType.ARRAY,
          value: [{ type: RuntimeType.STRING, value: expected }],
        });
      });

      describe('reassign', () => {
        it.each([
          { input: 'const a = 5; a = 10' },
          { input: 'const a = "5"; a = "10"' },
          { input: 'const a = true; a = false' },
          { input: 'const a = [1]; a = [2]' },
        ])('should throw when reassigning: $input', ({ input }) => {
          expect(() => evaluateAll(input)).toThrow();
        });
      });
    });

    describe('undeclared variables', () => {
      it('should throw when looking up an undeclared variable', () => {
        expect(() => evaluate('x')).toThrow();
      });

      it('should throw when assigning to an undeclared variable', () => {
        expect(() => evaluateAll('x = 5')).toThrow();
      });
    });

    describe('block statements', () => {
      it('should evaluate a block and return void', () => {
        const result = evaluate('{}');
        expect(result).toEqual({ type: RuntimeType.VOID, value: null });
      });

      it('should create an inner scope — variables do not leak out', () => {
        expect(() => evaluateAll('{ let x = 5 }; x')).toThrow();
      });

      it('should access outer scope variables from inside block', () => {
        const result = evaluateAll('let x = 10; { x }');
        expect(result).toEqual({ type: RuntimeType.VOID, value: null });
      });

      it('should shadow outer variable inside block without mutating it', () => {
        const result = evaluateAll('let x = 1; { let x = 99 }; x');
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 1 });
      });

      it('should mutate outer variable from inside block', () => {
        const result = evaluateAll('let x = 1; { x = 2 }; x');
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 2 });
      });
    });

    describe('function', () => {
      describe('function declaration', () => {
        it('should return void on declaration', () => {
          const result = evaluate('fn add(a, b) { a + b }');
          expect(result).toEqual({ type: RuntimeType.VOID, value: null });
        });

        it('should store function in the environment under its name', () => {
          const result = evaluateAll('fn add(a, b) { a + b }; add') as FunctionValue;
          expect(result.type).toBe(RuntimeType.FUNCTION);
          expect(result.parameters).toHaveLength(2);
          expect(result.parameters[0].value).toBe('a');
          expect(result.parameters[1].value).toBe('b');
        });

        it.each([
          { input: 'fn double(x) { x * 2 }; double', params: ['x'] },
          { input: 'fn add(a, b) { a + b }; add', params: ['a', 'b'] },
          { input: 'fn sum(a, b, c) { a + b + c }; sum', params: ['a', 'b', 'c'] },
          { input: 'fn greet() {}; greet', params: [] },
        ])('should store function with correct parameters for $input', ({ input, params }) => {
          const result = evaluateAll(input) as FunctionValue;
          expect(result.type).toBe(RuntimeType.FUNCTION);
          expect(result.parameters).toHaveLength(params.length);
          params.forEach((name, i) => {
            expect(result.parameters[i].value).toBe(name);
          });
        });
      });

      describe('function call', () => {
        it.each([
          { input: 'fn double(x) { x * 2 }; double(5)' },
          { input: 'fn add(a, b) { a + b }; add(3, 4)' },
          { input: 'fn greet() {}; greet()' },
        ])('should return void when function has no explicit return for $input', ({ input }) => {
          const result = evaluateAll(input);
          expect(result).toEqual({ type: RuntimeType.VOID, value: null });
        });

        it('should not leak function parameters into outer scope', () => {
          expect(() => evaluateAll('fn add(a, b) { a + b }; add(1, 2); a')).toThrow();
        });

        it('should throw when calling an undeclared function', () => {
          expect(() => evaluateAll('unknown()')).toThrow();
        });

        it('should throw when calling a non-function value', () => {
          expect(() => evaluateAll('let x = 5; x()')).toThrow();
        });

        it('should throw when called with too few arguments', () => {
          expect(() => evaluateAll('fn add(a, b) { a + b }; add(1)')).toThrow();
        });

        it('should throw when called with too many arguments', () => {
          expect(() => evaluateAll('fn double(x) { x * 2 }; double(1, 2)')).toThrow();
        });

        it('should support chained calls', () => {
          const input = 'fn add(a, b) { return a + b }; let x = add(1, 2); add(x, 4)';
          const result = evaluateAll(input);
          expect(result).toEqual({ type: RuntimeType.NUMBER, value: 7 });
        });

        it.each([
          {
            desc: 'number',
            input: 'let n = 10; fn addN(x) { return x + n }; addN(5)',
            expected: { type: RuntimeType.NUMBER, value: 15 },
          },
          {
            desc: 'string',
            input: 'let greeting = "hi"; fn getGreeting() { return greeting }; getGreeting()',
            expected: { type: RuntimeType.STRING, value: 'hi' },
          },
          {
            desc: 'boolean',
            input: 'let flag = true; fn getFlag() { return flag }; getFlag()',
            expected: { type: RuntimeType.BOOLEAN, value: true },
          },
          {
            desc: 'array',
            input: 'let arr = [1, 2]; fn getArr() { return arr }; getArr()',
            expected: {
              type: RuntimeType.ARRAY,
              value: [
                { type: RuntimeType.NUMBER, value: 1 },
                { type: RuntimeType.NUMBER, value: 2 },
              ],
            },
          },
          {
            desc: 'null',
            input: 'let x = nil; fn getX() { return x }; getX()',
            expected: { type: RuntimeType.NULL, value: null },
          },
        ])('should close over $desc outer scope variable', ({ input, expected }) => {
          expect(evaluateAll(input)).toEqual(expected);
        });
      });

      describe('return statements', () => {
        it.each([
          {
            input: 'fn double(x) { return x * 2 }; double(5)',
            expected: 10,
            type: RuntimeType.NUMBER,
          },
          {
            input: 'fn add(a, b) { return a + b }; add(3, 4)',
            expected: 7,
            type: RuntimeType.NUMBER,
          },
          {
            input: 'fn square(x) { return x * x }; square(4)',
            expected: 16,
            type: RuntimeType.NUMBER,
          },
          {
            input: 'fn sum(a, b, c) { return a + b + c }; sum(1, 2, 3)',
            expected: 6,
            type: RuntimeType.NUMBER,
          },
          {
            input: 'fn greet(name) { return name }; greet("hi")',
            expected: 'hi',
            type: RuntimeType.STRING,
          },
          { input: 'fn yes() { return true }; yes()', expected: true, type: RuntimeType.BOOLEAN },
          { input: 'fn no() { return false }; no()', expected: false, type: RuntimeType.BOOLEAN },
          {
            input: 'fn nothing() { return nil }; nothing()',
            expected: null,
            type: RuntimeType.NULL,
          },
        ])('should return expression value from function for $input', ({
          input,
          expected,
          type,
        }) => {
          const result = evaluateAll(input);
          expect(result).toEqual({ type, value: expected });
        });

        it('should return early and not evaluate subsequent statements', () => {
          const input = 'fn first(a, b) { return a; b }; first(1, 2)';
          const result = evaluateAll(input);
          expect(result).toEqual({ type: RuntimeType.NUMBER, value: 1 });
        });

        it('should return from nested block inside function', () => {
          const input = 'fn f(x) { { return x * 3 } }; f(4)';
          const result = evaluateAll(input);
          expect(result).toEqual({ type: RuntimeType.NUMBER, value: 12 });
        });

        it('should return a call expression result', () => {
          const input =
            'fn double(x) { return x * 2 }; fn quad(x) { return double(double(x)) }; quad(3)';
          const result = evaluateAll(input);
          expect(result).toEqual({ type: RuntimeType.NUMBER, value: 12 });
        });

        it('should throw when return is used outside a function', () => {
          expect(() => evaluateAll('return 5')).toThrow();
        });
      });
    });
  });

  describe('truthiness', () => {
    it.each([
      { input: 'true', expected: 1 },
      { input: 'false', expected: 0 },
      { input: 'nil', expected: 0 },
      { input: '1', expected: 1 },
      { input: '0', expected: 0 },
      { input: '"hello"', expected: 1 },
      { input: '""', expected: 0 },
      { input: '[1]', expected: 1 },
      { input: '[]', expected: 0 },
    ])('should evaluate truthiness of $input to $expected', ({ input, expected }) => {
      const result = evaluateAll(`let res = 0; if (${input}) { res = 1 } else { res = 0 }; res`);
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
    });

    describe('prefix expressions', () => {
      describe('!', () => {
        it.each([
          { input: '!true', expected: false },
          { input: '!false', expected: true },
          { input: '!nil', expected: true },
          { input: '!1', expected: false },
          { input: '!0', expected: true },
          { input: '!"hello"', expected: false },
          { input: '!""', expected: true },
          { input: '![1]', expected: false },
          { input: '![]', expected: true },
        ])('should evaluate $input to $expected', ({ input, expected }) => {
          expect(evaluate(input)).toEqual({ type: RuntimeType.BOOLEAN, value: expected });
        });
      });

      describe('!!', () => {
        it.each([
          { input: '!!true', expected: true },
          { input: '!!false', expected: false },
          { input: '!!nil', expected: false },
          { input: '!!1', expected: true },
          { input: '!!0', expected: false },
          { input: '!!"hello"', expected: true },
          { input: '!!""', expected: false },
          { input: '!![1]', expected: true },
          { input: '!![]', expected: false },
        ])('should evaluate $input to $expected', ({ input, expected }) => {
          expect(evaluate(input)).toEqual({ type: RuntimeType.BOOLEAN, value: expected });
        });
      });
    });
  });

  describe('if statements', () => {
    it.each([
      {
        input: 'let x = 0; if (true) { x = 10 }; x',
        expected: { type: RuntimeType.NUMBER, value: 10 },
      },
      {
        input: 'let x = 0; if (false) { x = 10 }; x',
        expected: { type: RuntimeType.NUMBER, value: 0 },
      },
      {
        input: 'let x = 0; if (1 < 2) { x = 10 } else { x = 20 }; x',
        expected: { type: RuntimeType.NUMBER, value: 10 },
      },
      {
        input: 'let x = 0; if (1 > 2) { x = 10 } else { x = 20 }; x',
        expected: { type: RuntimeType.NUMBER, value: 20 },
      },
      {
        input: 'let x = 0; if (1 > 2) { x = 10 } elif (1 < 2) { x = 5 } else { x = 0 }; x',
        expected: { type: RuntimeType.NUMBER, value: 5 },
      },
      {
        input: 'let x = 0; if (1 > 2) { x = 10 } elif (1 > 3) { x = 5 } else { x = 0 }; x',
        expected: { type: RuntimeType.NUMBER, value: 0 },
      },
    ])('should evaluate if/elif/else side effects: $input', ({ input, expected }) => {
      expect(evaluateAll(input)).toEqual(expected);
    });

    it.each([
      {
        input: 'fn f(x) { if (x > 10) { return "high" } else { return "low" } }; f(15)',
        expected: { type: RuntimeType.STRING, value: 'high' },
      },
      {
        input: 'fn f(x) { if (x > 10) { return "high" } else { return "low" } }; f(5)',
        expected: { type: RuntimeType.STRING, value: 'low' },
      },
      {
        input:
          'fn f(x) { if (x > 10) { return "high" } elif (x > 5) { return "medium" } else { return "low" } }; f(7)',
        expected: { type: RuntimeType.STRING, value: 'medium' },
      },
    ])('should support return from inside if/elif/else block: $input', ({ input, expected }) => {
      expect(evaluateAll(input)).toEqual(expected);
    });
  });

  describe('should throw an error', () => {
    it('should throw error for unknown node kind', () => {
      const unknownNode = { kind: 'UNKNOWN' } as any;
      const env = new Environment();
      expect(() => interpreter.eval(unknownNode, env)).toThrow(
        'AST have no implementation {"kind":"UNKNOWN"}',
      );
    });
  });

  describe('semicolon as statement separator', () => {
    it.each([
      { newline: 'let a = 5\na', semi: 'let a = 5; a' },
      { newline: 'let a = 5\nlet b = 10\nb', semi: 'let a = 5; let b = 10; b' },
      { newline: 'const x = 42\nx', semi: 'const x = 42; x' },
      { newline: 'let a = [1, 2]\na', semi: 'let a = [1, 2]; a' },
      { newline: 'let a = "hello"\na', semi: 'let a = "hello"; a' },
      { newline: 'let a = true\na', semi: 'let a = true; a' },
      { newline: 'let a = nil\na', semi: 'let a = nil; a' },
    ])('should produce same result for "$semi"', ({ newline, semi }) => {
      expect(evaluateAll(semi)).toEqual(evaluateAll(newline));
    });
  });
});
