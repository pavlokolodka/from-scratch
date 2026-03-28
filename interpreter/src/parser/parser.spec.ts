import type {
  AssignStatement,
  BlockStatement,
  CallExpression,
  ExpressionStatement,
  Identifier,
  InfixExpression,
  Node,
  Program,
} from './ast';
import { Lexer } from '../lexer/lexer';
import { ConstStatement, FunctionDeclaration, LetStatement, NodeKind, NumberLiteral } from './ast';
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
    case NodeKind.BLOCK_STATEMENT: {
      const block = node as BlockStatement;
      return `(block ${block.statements.map(stringify).join(' ')})`;
    }
    case NodeKind.FUNCTION_DECLARATION: {
      const fn = node as FunctionDeclaration;
      const params = fn.parameters.map((p) => p.value).join(' ');
      return `(fn ${fn.name.value} (${params}) ${stringify(fn.body)})`;
    }
    case NodeKind.CALL_EXPRESSION: {
      const call = node as CallExpression;
      const args = call.args.map(stringify).join(' ');
      return `(call ${call.identifier.value} (${args}))`;
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

  describe('block statements', () => {
    it.each([
      { input: '{}', expected: '(block )' },
      { input: '{ 5 }', expected: '(block 5)' },
      { input: '{ let x = 5 }', expected: '(block (let x 5))' },
      { input: '{ let x = 5\nx }', expected: '(block (let x 5) x)' },
      { input: '{ let x = 1\nlet y = 2\nx + y }', expected: '(block (let x 1) (let y 2) (+ x y))' },
    ])('should parse block statement $input to $expected', ({ input, expected }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(stringify(program)).toBe(expected);
    });

    it('should throw for unclosed block', () => {
      const lexer = new Lexer('{ let x = 5');
      const parser = new Parser(lexer.tokenize());
      expect(() => parser.parse()).toThrow();
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

  describe('function declarations', () => {
    it.each([
      { input: 'fn greet() {}', expected: '(fn greet () (block ))' },
      { input: 'fn double(x) { x * 2 }', expected: '(fn double (x) (block (* x 2)))' },
      { input: 'fn add(a, b) { a + b }', expected: '(fn add (a b) (block (+ a b)))' },
      {
        input: 'fn sum(a, b, c) { a + b + c }',
        expected: '(fn sum (a b c) (block (+ (+ a b) c)))',
      },
    ])('should parse function declaration $input to $expected', ({ input, expected }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(stringify(program)).toBe(expected);
    });

    it('should parse function name and parameters', () => {
      const lexer = new Lexer('fn add(a, b) { a + b }');
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(program.statements.length).toBe(1);
      const stmt = program.statements[0];
      expect(stmt.tokenLiteral()).toBe('fn');
      expect(stmt instanceof FunctionDeclaration).toBe(true);

      const fn = stmt as FunctionDeclaration;
      expect(fn.name.value).toBe('add');
      expect(fn.parameters.length).toBe(2);
      expect(fn.parameters[0].value).toBe('a');
      expect(fn.parameters[1].value).toBe('b');
    });

    it('should parse function with no parameters', () => {
      const lexer = new Lexer('fn greet() {}');
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      const fn = program.statements[0] as FunctionDeclaration;
      expect(fn.parameters.length).toBe(0);
      expect(fn.name.value).toBe('greet');
    });

    it('should parse function with one parameter', () => {
      const lexer = new Lexer('fn double(x) { x * 2 }');
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      const fn = program.statements[0] as FunctionDeclaration;
      expect(fn.parameters.length).toBe(1);
      expect(fn.parameters[0].value).toBe('x');
    });

    it('should throw for unclosed function body', () => {
      const lexer = new Lexer('fn add(a, b) { a + b');
      const parser = new Parser(lexer.tokenize());
      expect(() => parser.parse()).toThrow();
    });

    it.each([
      { input: 'fn () {}', desc: 'missing function name' },
      { input: 'fn {}', desc: 'missing name and parameter list' },
      { input: 'fn add {}', desc: 'missing parameter list' },
    ])('should throw for $desc: $input', ({ input }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      expect(() => parser.parse()).toThrow();
    });

    it.each([
      { input: 'fn add(a,) {}', desc: 'trailing comma, no param after comma' },
      { input: 'fn add(a, ) {}', desc: 'trailing comma with space, no param after comma' },
      { input: 'fn add(, b) {}', desc: 'leading comma before first param' },
      { input: 'fn add(a,, b) {}', desc: 'double comma between params' },
    ])('should throw for $desc: $input', ({ input }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      expect(() => parser.parse()).toThrow();
    });

    it.each([
      { input: 'fn add(a, b {}', desc: 'unclosed param list (missing closing paren)' },
      { input: 'fn add(a, b', desc: 'unclosed param list, EOF' },
      { input: 'fn add(', desc: 'only opening paren, EOF' },
    ])('should throw for $desc: $input', ({ input }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      expect(() => parser.parse()).toThrow();
    });

    it.each([
      { input: 'fn add(123) {}', desc: 'number literal as parameter' },
      { input: 'fn add(a + b) {}', desc: 'expression as parameter' },
    ])('should throw for $desc: $input', ({ input }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      expect(() => parser.parse()).toThrow();
    });
  });

  describe('function call expressions', () => {
    it.each([
      { input: 'greet()', expected: '(call greet ())' },
      { input: 'double(5)', expected: '(call double (5))' },
      { input: 'add(1, 2)', expected: '(call add (1 2))' },
      { input: 'sum(1, 2, 3)', expected: '(call sum (1 2 3))' },
      { input: 'add(a, b)', expected: '(call add (a b))' },
      { input: 'add(1 + 2, 3)', expected: '(call add ((+ 1 2) 3))' },
      { input: 'add(x * 2, y)', expected: '(call add ((* x 2) y))' },
    ])('should parse call expression $input to $expected', ({ input, expected }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(stringify(program)).toBe(expected);
    });

    it('should parse call expression AST node fields', () => {
      const lexer = new Lexer('add(1, 2)');
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      expect(program.statements.length).toBe(1);
      const stmt = program.statements[0] as ExpressionStatement;
      const call = stmt.expression as CallExpression;

      expect(call.kind).toBe(NodeKind.CALL_EXPRESSION);
      expect(call.identifier.value).toBe('add');
      expect(call.args).toHaveLength(2);
    });

    it.each([
      { input: 'add(1,)', desc: 'trailing comma in args' },
      { input: 'add(,1)', desc: 'leading comma in args' },
      { input: 'add(1,,2)', desc: 'double comma in args' },
    ])('should throw for $desc: $input', ({ input }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      expect(() => parser.parse()).toThrow();
    });

    it.each([
      { input: 'add(1, 2', desc: 'unclosed arg list, EOF' },
      { input: 'add(', desc: 'only opening paren, EOF' },
    ])('should throw for $desc: $input', ({ input }) => {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      expect(() => parser.parse()).toThrow();
    });
  });
});
