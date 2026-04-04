import type {
  AssignStatement,
  BlockStatement,
  CallExpression,
  ConstStatement,
  ExpressionStatement,
  FunctionDeclaration,
  Identifier,
  InfixExpression,
  LetStatement,
  Node,
  NumberLiteral,
  Program,
  ReturnStatement,
  StringLiteral,
} from './ast';
import { Lexer } from '../lexer/lexer';
import { NodeKind } from './ast';
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
    case NodeKind.NUMBER_LITERAL: {
      const num = node as NumberLiteral;
      expect(num.value).toEqual(expect.any(Number));
      return `${num.value}`;
    }
    case NodeKind.STRING_LITERAL: {
      const str = node as StringLiteral;
      expect(str.value).toEqual(expect.any(String));
      return `"${str.value}"`;
    }
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
    case NodeKind.RETURN_STATEMENT: {
      const ret = node as ReturnStatement;
      return `(return ${stringify(ret.value)})`;
    }
    default:
      return '';
  }
}

function parse(input: string): Program {
  return new Parser(new Lexer(input).tokenize()).parse();
}

describe('Parser', () => {
  describe('number literals', () => {
    it.each([
      { input: '1', expected: '1' },
      { input: '10', expected: '10' },
      { input: '100000', expected: '100000' },
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });
  });

  describe('string literals', () => {
    it.each([
      { input: '"hello"', expected: '"hello"' },
      { input: '"world"', expected: '"world"' },
      { input: '""', expected: '""' },
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });
  });

  describe('let statements', () => {
    it.each([
      { input: 'let x = 5;', expected: '(let x 5)' },
      { input: 'let result = 2 + 3;', expected: '(let result (+ 2 3))' },
      { input: 'let z = x * y;', expected: '(let z (* x y))' },
      { input: 'let s = "hello";', expected: '(let s "hello")' },
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it.each([
      { input: 'let = 5;', desc: 'missing identifier' },
      { input: 'let x;', desc: 'missing value' },
    ])('should throw for $desc: $input', ({ input }) => {
      expect(() => parse(input)).toThrow();
    });
  });

  describe('const statements', () => {
    it.each([
      { input: 'const x = 5;', expected: '(const x 5)' },
      { input: 'const result = 2 + 3;', expected: '(const result (+ 2 3))' },
      { input: 'const z = x * y;', expected: '(const z (* x y))' },
      { input: 'const greeting = "hi";', expected: '(const greeting "hi")' },
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it.each([
      { input: 'const = 5;', desc: 'missing identifier' },
      { input: 'const x;', desc: 'missing value' },
    ])('should throw for $desc: $input', ({ input }) => {
      expect(() => parse(input)).toThrow();
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
      { input: 'x = "hello";', expected: '(= x "hello")' },
      { input: 'x = "1";', expected: '(= x "1")' },
      { input: 'y = "";', expected: '(= y "")' },
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it('should throw for missing right-hand side', () => {
      expect(() => parse('x =')).toThrow();
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
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it.each([
      { input: '(5', desc: 'unclosed paren' },
      { input: '(5 + 5', desc: 'unclosed paren around expression' },
      { input: '((5 + 5)', desc: 'double-nested unclosed paren' },
      { input: '5 + (5 * 5', desc: 'unclosed paren in right operand' },
      { input: '(2 * (3 + 4', desc: 'double-nested unclosed parens' },
    ])('should throw for $desc: $input', ({ input }) => {
      expect(() => parse(input)).toThrow('Closing parentheses not found');
    });
  });

  describe('block statements', () => {
    it.each([
      { input: '{}', expected: '(block )' },
      { input: '{ 5 }', expected: '(block 5)' },
      { input: '{ let x = 5 }', expected: '(block (let x 5))' },
      { input: '{ let x = 5\nx }', expected: '(block (let x 5) x)' },
      { input: '{ let x = 1\nlet y = 2\nx + y }', expected: '(block (let x 1) (let y 2) (+ x y))' },
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it('should throw for unclosed block', () => {
      expect(() => parse('{ let x = 5')).toThrow();
    });
  });

  describe('identifier expressions', () => {
    it.each([
      { input: 'a;', expected: 'a' },
      { input: 'b;', expected: 'b' },
      { input: 'foobar;', expected: 'foobar' },
      { input: 'a + b;', expected: '(+ a b)' },
      { input: 'x * y + z;', expected: '(+ (* x y) z)' },
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });
  });

  describe('function declarations', () => {
    describe('syntax', () => {
      it.each([
        { input: 'fn greet() {}', expected: '(fn greet () (block ))' },
        { input: 'fn double(x) { x * 2 }', expected: '(fn double (x) (block (* x 2)))' },
        { input: 'fn add(a, b) { a + b }', expected: '(fn add (a b) (block (+ a b)))' },
        {
          input: 'fn sum(a, b, c) { a + b + c }',
          expected: '(fn sum (a b c) (block (+ (+ a b) c)))',
        },
      ])('should stringify $input to $expected', ({ input, expected }) => {
        expect(stringify(parse(input))).toBe(expected);
      });

      it.each([
        { input: 'fn () {}', desc: 'missing function name' },
        { input: 'fn {}', desc: 'missing name and parameter list' },
        { input: 'fn add {}', desc: 'missing parameter list' },
      ])('should throw for $desc: $input', ({ input }) => {
        expect(() => parse(input)).toThrow();
      });

      it.each([
        { input: 'fn add(a,) {}', desc: 'trailing comma' },
        { input: 'fn add(a, ) {}', desc: 'trailing comma with space' },
        { input: 'fn add(, b) {}', desc: 'leading comma' },
        { input: 'fn add(a,, b) {}', desc: 'double comma' },
      ])('should throw for $desc in param list: $input', ({ input }) => {
        expect(() => parse(input)).toThrow();
      });

      it.each([
        { input: 'fn add(a, b {}', desc: 'missing closing paren' },
        { input: 'fn add(a, b', desc: 'unclosed param list, EOF' },
        { input: 'fn add(', desc: 'only opening paren, EOF' },
      ])('should throw for unclosed param list ($desc): $input', ({ input }) => {
        expect(() => parse(input)).toThrow();
      });

      it.each([
        { input: 'fn add(123) {}', desc: 'number literal as parameter' },
        { input: 'fn add(a + b) {}', desc: 'expression as parameter' },
      ])('should throw for non-identifier param ($desc): $input', ({ input }) => {
        expect(() => parse(input)).toThrow();
      });

      it('should throw for unclosed function body', () => {
        expect(() => parse('fn add(a, b) { a + b')).toThrow();
      });
    });

    describe('return', () => {
      it.each([
        {
          input: 'fn double(x) { return x * 2 }',
          expected: '(fn double (x) (block (return (* x 2))))',
        },
        {
          input: 'fn greet() { return "hello" }',
          expected: '(fn greet () (block (return "hello")))',
        },
      ])('should stringify return inside fn body: $input', ({ input, expected }) => {
        expect(stringify(parse(input))).toBe(expected);
      });

      it('should parse early return before other statements', () => {
        expect(stringify(parse('fn first(a, b) { return a\nb }'))).toBe(
          '(fn first (a b) (block (return a) b))',
        );
      });
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
      { input: 'greet("world")', expected: '(call greet ("world"))' },
      { input: 'concat("hello", "world")', expected: '(call concat ("hello" "world"))' },
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it.each([
      { input: 'add(1,)', desc: 'trailing comma in args' },
      { input: 'add(,1)', desc: 'leading comma in args' },
      { input: 'add(1,,2)', desc: 'double comma in args' },
    ])('should throw for $desc: $input', ({ input }) => {
      expect(() => parse(input)).toThrow();
    });

    it.each([
      { input: 'add(1, 2', desc: 'unclosed arg list, EOF' },
      { input: 'add(', desc: 'only opening paren, EOF' },
    ])('should throw for unclosed arg list ($desc): $input', ({ input }) => {
      expect(() => parse(input)).toThrow();
    });
  });

  describe('return statements', () => {
    it.each([
      { input: 'return 5', expected: '(return 5)' },
      { input: 'return a', expected: '(return a)' },
      { input: 'return a + b', expected: '(return (+ a b))' },
      { input: 'return x * 2', expected: '(return (* x 2))' },
      { input: 'return add(1, 2)', expected: '(return (call add (1 2)))' },
      { input: 'return "hello"', expected: '(return "hello")' },
    ])('should stringify $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it('should parse return statement AST node fields', () => {
      const stmt = parse('return 5').statements[0] as ReturnStatement;
      expect(stmt.tokenLiteral()).toBe('return');
      expect((stmt.value as NumberLiteral).value).toBe(5);
    });

    it('should throw for return with no expression', () => {
      expect(() => parse('return')).toThrow();
    });
  });
});
