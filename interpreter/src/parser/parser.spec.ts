import type {
  AssignStatement,
  ExpressionStatement,
  Identifier,
  InfixExpression,
  Node,
  Program,
} from './ast';
import { Lexer } from '../lexer/lexer';
import { ConstStatement, LetStatement, NodeKind, NumberLiteral } from './ast';
import { Parser } from './parser';

function stringify(node: Node): string {
  switch (node.kind) {
    case NodeKind.PROGRAM:
      return (node as Program).statements.map(stringify).join('');
    case NodeKind.EXPRESSION_STATEMENT:
      return stringify((node as ExpressionStatement).expression);
    case NodeKind.ASSIGN_STATEMENT: {
      const assign = node as AssignStatement;
      return `(= ${assign.left.value} ${stringify(assign.right)})`;
    }
    case NodeKind.INFIX_EXPRESSION: {
      const infix = node as InfixExpression;
      return `(${infix.operator} ${stringify(infix.left)} ${stringify(infix.right)})`;
    }
    case NodeKind.NUMBER_LITERAL:
      return (node as NumberLiteral).tokenLiteral();
    case NodeKind.IDENTIFIER:
      return (node as Identifier).value;
    case NodeKind.LET_STATEMENT: {
      const letStmt = node as LetStatement;
      return `(let ${letStmt.left.value} ${stringify(letStmt.right)})`;
    }
    case NodeKind.CONST_STATEMENT: {
      const constStmt = node as ConstStatement;
      return `(const ${constStmt.left.value} ${stringify(constStmt.right)})`;
    }
    default:
      return '';
  }
}

describe('Parser', () => {
  describe('let statements', () => {
    it.each([
      { input: 'let x = 5;', name: 'x', value: 5 },
      { input: 'let y = 10;', name: 'y', value: 10 },
      { input: 'let foobar = 838383;', name: 'foobar', value: 838383 },
    ])('should parse let statement $input', ({ input, name, value }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(program.statements.length).toBe(1);
      const stmt = program.statements[0];
      expect(stmt.tokenLiteral()).toBe('let');
      expect(stmt instanceof LetStatement).toBe(true);

      const letStmt = stmt as LetStatement;
      expect(letStmt.left.value).toBe(name);
      expect(letStmt.left.tokenLiteral()).toBe(name);

      expect(letStmt.right instanceof NumberLiteral).toBe(true);
      const val = letStmt.right as NumberLiteral;
      expect(val.value).toBe(value);
      expect(val.tokenLiteral()).toBe(String(value));
    });

    it.each([
      { input: 'let x = 5;', expected: '(let x 5)' },
      { input: 'let result = 2 + 3;', expected: '(let result (+ 2 3))' },
      { input: 'let z = x * y;', expected: '(let z (* x y))' },
    ])('should stringify let statement $input to $expected', ({ input, expected }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(stringify(program)).toBe(expected);
    });
  });

  describe('const statements', () => {
    it.each([
      { input: 'const x = 5;', name: 'x', value: 5 },
      { input: 'const y = 10;', name: 'y', value: 10 },
      { input: 'const foobar = 838383;', name: 'foobar', value: 838383 },
    ])('should parse const statement $input', ({ input, name, value }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(program.statements.length).toBe(1);
      const stmt = program.statements[0];
      expect(stmt.tokenLiteral()).toBe('const');
      expect(stmt instanceof ConstStatement).toBe(true);

      const constStmt = stmt as ConstStatement;
      expect(constStmt.left.value).toBe(name);
      expect(constStmt.left.tokenLiteral()).toBe(name);

      expect(constStmt.right instanceof NumberLiteral).toBe(true);
      const val = constStmt.right as NumberLiteral;
      expect(val.value).toBe(value);
      expect(val.tokenLiteral()).toBe(String(value));
    });

    it.each([
      { input: 'const x = 5;', expected: '(const x 5)' },
      { input: 'const result = 2 + 3;', expected: '(const result (+ 2 3))' },
      { input: 'const z = x * y;', expected: '(const z (* x y))' },
    ])('should stringify const statement $input to $expected', ({ input, expected }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(stringify(program)).toBe(expected);
    });
  });

  describe('assign statements', () => {
    it.each([
      { input: 'x = 5;', expected: '(= x 5)' },
      { input: 'y = 10 + 5;', expected: '(= y (+ 10 5))' },
      { input: 'foobar = x * y;', expected: '(= foobar (* x y))' },
      { input: 'x = y;', expected: '(= x y)' },
      { input: 'x = x + 1;', expected: '(= x (+ x 1))' },
      { input: 'a = b * c + d;', expected: '(= a (+ (* b c) d))' },
    ])('should parse assign statement $input to $expected', ({ input, expected }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(stringify(program)).toBe(expected);
    });
  });

  describe('infix expressions', () => {
    it.each([
      { input: '1 + 2', expected: '(+ 1 2)' },
      { input: '1 - 2', expected: '(- 1 2)' },
      { input: '1 * 2', expected: '(* 1 2)' },
      { input: '1 / 2', expected: '(/ 1 2)' },

      { input: '1 + 2 + 3', expected: '(+ (+ 1 2) 3)' },
      { input: '1 - 2 - 3', expected: '(- (- 1 2) 3)' },
      { input: '1 * 2 * 3', expected: '(* (* 1 2) 3)' },
      { input: '1 / 2 / 3', expected: '(/ (/ 1 2) 3)' },
      { input: '1 + 2 * 3', expected: '(+ 1 (* 2 3))' },
      { input: '1 * 2 + 3', expected: '(+ (* 1 2) 3)' },
      { input: '1 - 2 / 3', expected: '(- 1 (/ 2 3))' },
      { input: '1 / 2 - 3', expected: '(- (/ 1 2) 3)' },

      { input: '1 + 2 + 3 + 4', expected: '(+ (+ (+ 1 2) 3) 4)' },
      { input: '1 - 2 - 3 - 4', expected: '(- (- (- 1 2) 3) 4)' },
      { input: '1 * 2 * 3 * 4', expected: '(* (* (* 1 2) 3) 4)' },
      { input: '1 / 2 / 3 / 4', expected: '(/ (/ (/ 1 2) 3) 4)' },

      { input: '1 + 2 * 3 + 4', expected: '(+ (+ 1 (* 2 3)) 4)' },
      { input: '1 * 2 + 3 * 4', expected: '(+ (* 1 2) (* 3 4))' },
      { input: '1 + 2 / 3 + 4', expected: '(+ (+ 1 (/ 2 3)) 4)' },
      { input: '1 / 2 + 3 / 4', expected: '(+ (/ 1 2) (/ 3 4))' },

      { input: '1 - 2 * 3 - 4', expected: '(- (- 1 (* 2 3)) 4)' },
      { input: '1 * 2 - 3 * 4', expected: '(- (* 1 2) (* 3 4))' },
      { input: '1 - 2 / 3 - 4', expected: '(- (- 1 (/ 2 3)) 4)' },
      { input: '1 / 2 - 3 / 4', expected: '(- (/ 1 2) (/ 3 4))' },

      { input: '1 * (2 + 3)', expected: '(* 1 (+ 2 3))' },
      { input: '(1 + 2) * 3', expected: '(* (+ 1 2) 3)' },
      { input: '(1 * (2 + 3))', expected: '(* 1 (+ 2 3))' },
      { input: '((1 + 2) * 3)', expected: '(* (+ 1 2) 3)' },
      { input: '((1 + 2) * 3) * 4', expected: '(* (* (+ 1 2) 3) 4)' },
    ])('should parse infix expression $input to $expected', ({ input, expected }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(stringify(program)).toBe(expected);
    });
  });

  describe('identifier expressions', () => {
    it.each([
      { input: 'a;', expected: 'a' },
      { input: 'b;', expected: 'b' },
      { input: 'foobar;', expected: 'foobar' },
      { input: 'a + b;', expected: '(+ a b)' },
      { input: 'x * y + z;', expected: '(+ (* x y) z)' },
    ])('should parse identifier expression $input to $expected', ({ input, expected }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(stringify(program)).toBe(expected);
    });
  });

  describe('should throw error', () => {
    it.each([
      { input: '(5' },
      { input: '(5 + 5' },
      { input: '((5 + 5)' },
      { input: '5 + (5 * 5' },
      { input: '(2 * (3 + 4' },
    ])('should throw error for missing closing parenthesis in $input', ({ input }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());

      expect(() => parser.parse()).toThrow('Closing parentheses not found');
    });
  });
});
