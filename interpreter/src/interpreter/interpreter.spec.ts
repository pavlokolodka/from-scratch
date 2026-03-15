import { Lexer } from '../lexer/lexer';
import { Parser } from '../parser/parser';
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
    const program = parser.parse();

    const stmt = program.statements[0];

    return interpreter.eval(stmt);
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

  describe('should throw an error', () => {
    it('should throw error for unknown node kind', () => {
      const unknownNode = { kind: 'UNKNOWN' } as any;
      expect(() => interpreter.eval(unknownNode)).toThrow(
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
