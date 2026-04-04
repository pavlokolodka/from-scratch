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
        { input: '(5' },
        { input: '(5 + 5' },
        { input: '((5 + 5)' },
        { input: '5 + (5 * 5' },
        { input: '(2 * (3 + 4' },
      ])('should throw error for missing closing parenthesis in $input', ({ input }) => {
        expect(() => evaluate(input)).toThrow('Closing parentheses not found');
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
      const result = evaluateAll('let s = "hello"\ns') as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'hello' });
    });

    it('should store a string in a const variable', () => {
      const result = evaluateAll('const greeting = "hi"\ngreeting') as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'hi' });
    });

    it('should reassign a string variable', () => {
      const result = evaluateAll('let s = "hello"\ns = "world"\ns') as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'world' });
    });

    it('should pass a string as a function argument', () => {
      const input = 'fn identity(x) { return x }\nidentity("hello")';
      const result = evaluateAll(input) as StringValue;
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'hello' });
    });

    it('should return a string from a function', () => {
      const input = 'fn greet() { return "hello" }\ngreet()';
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

  describe('let declaration', () => {
    describe('number', () => {
      it.each([
        { input: 'let a = 5\na', expected: 5 },
        { input: 'let b = 10\nb', expected: 10 },
        { input: 'let c = 2 + 3\nc', expected: 5 },
        { input: 'let d = 5\nlet e = 10\nd', expected: 5 },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
      });
    });

    describe('string', () => {
      it.each([
        { input: 'let a = "5"\na', expected: '5' },
        { input: 'let b = "10"\nb', expected: '10' },
        { input: 'let c = "hello"\nc', expected: 'hello' },
        { input: 'let d = "a"\nlet e = "b"\nd', expected: 'a' },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.STRING, value: expected });
      });
    });

    describe('reassign', () => {
      it.each([
        { input: 'let a = 5\na = 10; a', expected: 10 },
        { input: 'let a = "5"\na = "10"; a', expected: '10' },
      ])('should throw when reassigning: $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result.value).toBe(expected);
      });
    });
  });

  describe('const declaration', () => {
    describe('number', () => {
      it.each([
        { input: 'const a = 5\na', expected: 5 },
        { input: 'const b = 10\nb', expected: 10 },
        { input: 'const c = 2 + 3\nc', expected: 5 },
        { input: 'const d = 5\nconst e = 10\nd', expected: 5 },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
      });
    });

    describe('string', () => {
      it.each([
        { input: 'const a = "5"\na', expected: '5' },
        { input: 'const b = "10"\nb', expected: '10' },
        { input: 'const c = "hello"\nc', expected: 'hello' },
        { input: 'const d = "a"\nconst e = "b"\nd', expected: 'a' },
      ])('should declare and look up $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.STRING, value: expected });
      });
    });

    describe('reassign', () => {
      it.each([
        { input: 'const a = 5\na = 10' },
        { input: 'const a = "5"\na = "10' },
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
      expect(() => evaluateAll('{ let x = 5 }\nx')).toThrow();
    });

    it('should access outer scope variables from inside block', () => {
      const result = evaluateAll('let x = 10\n{ x }');
      expect(result).toEqual({ type: RuntimeType.VOID, value: null });
    });

    it('should shadow outer variable inside block without mutating it', () => {
      const result = evaluateAll('let x = 1\n{ let x = 99 }\nx');
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: 1 });
    });

    it('should mutate outer variable from inside block', () => {
      const result = evaluateAll('let x = 1\n{ x = 2 }\nx');
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
        const result = evaluateAll('fn add(a, b) { a + b }\nadd') as FunctionValue;
        expect(result.type).toBe(RuntimeType.FUNCTION);
        expect(result.parameters).toHaveLength(2);
        expect(result.parameters[0].value).toBe('a');
        expect(result.parameters[1].value).toBe('b');
      });

      it.each([
        { input: 'fn double(x) { x * 2 }\ndouble', params: ['x'] },
        { input: 'fn add(a, b) { a + b }\nadd', params: ['a', 'b'] },
        { input: 'fn sum(a, b, c) { a + b + c }\nsum', params: ['a', 'b', 'c'] },
        { input: 'fn greet() {}\ngreet', params: [] },
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
        { input: 'fn double(x) { x * 2 }\ndouble(5)' },
        { input: 'fn add(a, b) { a + b }\nadd(3, 4)' },
        { input: 'fn greet() {}\ngreet()' },
      ])('should return void when function has no explicit return for $input', ({ input }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.VOID, value: null });
      });

      it('should not leak function parameters into outer scope', () => {
        expect(() => evaluateAll('fn add(a, b) { a + b }\nadd(1, 2)\na')).toThrow();
      });

      it('should throw when calling an undeclared function', () => {
        expect(() => evaluateAll('unknown()')).toThrow();
      });

      it('should throw when calling a non-function value', () => {
        expect(() => evaluateAll('let x = 5\nx()')).toThrow();
      });

      it('should throw when called with too few arguments', () => {
        expect(() => evaluateAll('fn add(a, b) { a + b }\nadd(1)')).toThrow();
      });

      it('should throw when called with too many arguments', () => {
        expect(() => evaluateAll('fn double(x) { x * 2 }\ndouble(1, 2)')).toThrow();
      });

      it('should support chained calls', () => {
        const input = 'fn add(a, b) { return a + b }\nlet x = add(1, 2)\nadd(x, 4)';
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 7 });
      });

      it('should close over outer scope variables', () => {
        const input = 'let n = 10\nfn addN(x) { return x + n }\naddN(5)';
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 15 });
      });
    });

    describe('return statements', () => {
      it.each([
        { input: 'fn double(x) { return x * 2 }\ndouble(5)', expected: 10 },
        { input: 'fn add(a, b) { return a + b }\nadd(3, 4)', expected: 7 },
        { input: 'fn square(x) { return x * x }\nsquare(4)', expected: 16 },
        { input: 'fn sum(a, b, c) { return a + b + c }\nsum(1, 2, 3)', expected: 6 },
      ])('should return expression value from function for $input', ({ input, expected }) => {
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
      });

      it('should return early and not evaluate subsequent statements', () => {
        const input = 'fn first(a, b) { return a\nb }\nfirst(1, 2)';
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 1 });
      });

      it('should return from nested block inside function', () => {
        const input = 'fn f(x) { { return x * 3 } }\nf(4)';
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 12 });
      });

      it('should return a call expression result', () => {
        const input =
          'fn double(x) { return x * 2 }\nfn quad(x) { return double(double(x)) }\nquad(3)';
        const result = evaluateAll(input);
        expect(result).toEqual({ type: RuntimeType.NUMBER, value: 12 });
      });

      it('should throw when return is used outside a function', () => {
        expect(() => evaluateAll('return 5')).toThrow();
      });
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
});
