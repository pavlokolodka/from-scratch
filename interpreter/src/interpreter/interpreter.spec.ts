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

  it.each([
    { input: '5', expected: 5 },
    { input: '10', expected: 10 },
  ])('should evaluate number literal $input to $expected', ({ input, expected }) => {
    const result = evaluate(input);
    expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
  });

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

  describe('let declaration', () => {
    it.each([
      { input: 'let x = 5\nx', expected: 5 },
      { input: 'let result = 10\nresult', expected: 10 },
      { input: 'let a = 2 + 3\na', expected: 5 },
      { input: 'let a = 5\nlet b = 10\nb', expected: 10 },
    ])('should declare and look up $input', ({ input, expected }) => {
      const result = evaluateAll(input);
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
    });

    it('should throw when looking up an undeclared variable', () => {
      expect(() => evaluate('x')).toThrow();
    });
  });

  describe('const declaration', () => {
    it.each([
      { input: 'const x = 5\nx', expected: 5 },
      { input: 'const result = 10\nresult', expected: 10 },
      { input: 'const a = 2 + 3\na', expected: 5 },
      { input: 'const a = 5\nconst b = 10\nb', expected: 10 },
    ])('should declare and look up $input', ({ input, expected }) => {
      const result = evaluateAll(input);
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
    });

    it('should throw when reassigning a const variable', () => {
      expect(() => evaluateAll('const x = 5\nx = 10')).toThrow();
    });
  });

  describe('reassignment', () => {
    it.each([
      { input: 'let x = 5\nx = 10\nx', expected: 10 },
      { input: 'let x = 1\nx = x + 1\nx', expected: 2 },
      { input: 'let a = 5\nlet b = 10\na = b\na', expected: 10 },
      { input: 'let x = 2\nx = x * 3\nx', expected: 6 },
    ])('should reassign and look up $input', ({ input, expected }) => {
      const result = evaluateAll(input);
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
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

  describe('should throw an error', () => {
    it('should throw error for unknown node kind', () => {
      const unknownNode = { kind: 'UNKNOWN' } as any;
      const env = new Environment();
      expect(() => interpreter.eval(unknownNode, env)).toThrow(
        'AST have no implementation {"kind":"UNKNOWN"}',
      );
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
