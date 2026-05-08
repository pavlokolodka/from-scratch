import type {
  ArrayLiteral,
  AssignStatement,
  BlockStatement,
  BooleanLiteral,
  BreakStatement,
  CallExpression,
  ConstStatement,
  ExpressionStatement,
  FunctionDeclaration,
  Identifier,
  IfStatement,
  IndexAssignStatement,
  IndexExpression,
  InfixExpression,
  LetStatement,
  Node,
  NullLiteral,
  NumberLiteral,
  PrefixExpression,
  Program,
  ReturnStatement,
  StringLiteral,
  WhileStatement,
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
    case NodeKind.PREFIX_EXPRESSION: {
      const prefix = node as PrefixExpression;
      return `(${prefix.operator}${stringify(prefix.right)})`;
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
    case NodeKind.BOOLEAN_LITERAL: {
      const bool = node as BooleanLiteral;
      return `${bool.value}`;
    }
    case NodeKind.NULL_LITERAL:
      return 'nil';
    case NodeKind.BREAK_STATEMENT:
      return 'stop';
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
    case NodeKind.IF_STATEMENT: {
      const ifStmt = node as IfStatement;
      let s = `(if ${stringify(ifStmt.condition)} ${stringify(ifStmt.body)}`;
      if (ifStmt.alternative) {
        s += ` ${stringify(ifStmt.alternative)}`;
      }
      s += ')';
      return s;
    }
    case NodeKind.WHILE_STATEMENT: {
      const whileStmt = node as WhileStatement;
      return `(while ${stringify(whileStmt.condition)} ${stringify(whileStmt.body)})`;
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
    case NodeKind.ARRAY_LITERAL: {
      const arr = node as ArrayLiteral;
      return `(array ${arr.elements.map(stringify).join(' ')})`;
    }
    case NodeKind.INDEX_EXPRESSION: {
      const idx = node as IndexExpression;
      return `(index ${stringify(idx.left)} ${stringify(idx.index)})`;
    }
    case NodeKind.INDEX_ASSIGN_STATEMENT: {
      const ia = node as IndexAssignStatement;
      return `(index-assign ${stringify(ia.left)} ${stringify(ia.right)})`;
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
    ])('should parse $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });
  });

  describe('string literals', () => {
    it.each([
      { input: '"hello"', expected: '"hello"' },
      { input: '"world"', expected: '"world"' },
      { input: '""', expected: '""' },
    ])('should parse $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });
  });

  describe('boolean literals', () => {
    it.each([
      { input: 'true', expected: 'true' },
      { input: 'false', expected: 'false' },
    ])('should parse $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it('should parse boolean AST node fields for true', () => {
      const program = parse('true');
      const expr = (program.statements[0] as ExpressionStatement).expression as BooleanLiteral;
      expect(expr.kind).toBe(NodeKind.BOOLEAN_LITERAL);
      expect(expr.value).toBe(true);
      expect(expr.tokenLiteral()).toBe('true');
    });

    it('should parse boolean AST node fields for false', () => {
      const program = parse('false');
      const expr = (program.statements[0] as ExpressionStatement).expression as BooleanLiteral;
      expect(expr.kind).toBe(NodeKind.BOOLEAN_LITERAL);
      expect(expr.value).toBe(false);
      expect(expr.tokenLiteral()).toBe('false');
    });

    it('should parse let declaration with boolean', () => {
      expect(stringify(parse('let flag = true;'))).toBe('(let flag true)');
      expect(stringify(parse('let flag = false;'))).toBe('(let flag false)');
    });

    it('should parse const declaration with boolean', () => {
      expect(stringify(parse('const debug = true;'))).toBe('(const debug true)');
    });
  });

  describe('null literal', () => {
    it('should parse nil to nil', () => {
      expect(stringify(parse('nil'))).toBe('nil');
    });

    it('should parse nil AST node fields', () => {
      const program = parse('nil');
      const expr = (program.statements[0] as ExpressionStatement).expression as NullLiteral;
      expect(expr.kind).toBe(NodeKind.NULL_LITERAL);
      expect(expr.tokenLiteral()).toBe('nil');
    });

    it('should parse let declaration with nil', () => {
      expect(stringify(parse('let x = nil;'))).toBe('(let x nil)');
    });

    it('should parse const declaration with nil', () => {
      expect(stringify(parse('const x = nil;'))).toBe('(const x nil)');
    });
  });

  describe('array', () => {
    describe('array literals', () => {
      it.each([
        { input: '[]', expected: '(array )' },
        { input: '[1, 2, 3]', expected: '(array 1 2 3)' },
        { input: '["a", "b"]', expected: '(array "a" "b")' },
        { input: '[x, y]', expected: '(array x y)' },
        { input: '[1 + 2, 3]', expected: '(array (+ 1 2) 3)' },
        { input: '[1, [2, 3]]', expected: '(array 1 (array 2 3))' },
      ])('should parse $input to $expected', ({ input, expected }) => {
        expect(stringify(parse(input))).toBe(expected);
      });

      it.each([
        { input: '[1, 2', desc: 'unclosed bracket' },
        { input: '[1,]', desc: 'trailing comma' },
        { input: '[,1]', desc: 'leading comma' },
        { input: '[1,,2]', desc: 'double comma' },
      ])('should throw for $desc: $input', ({ input }) => {
        expect(() => parse(input)).toThrow();
      });
    });

    describe('let statements', () => {
      it.each([
        { input: 'let x = 5;', expected: '(let x 5)' },
        { input: 'let result = 2 + 3;', expected: '(let result (+ 2 3))' },
        { input: 'let z = x * y;', expected: '(let z (* x y))' },
        { input: 'let s = "hello";', expected: '(let s "hello")' },
        { input: 'let a = [1, 2, 3];', expected: '(let a (array 1 2 3))' },
        { input: 'let a = ["x", "y"];', expected: '(let a (array "x" "y"))' },
        { input: 'let a = [];', expected: '(let a (array ))' },
      ])('should parse $input to $expected', ({ input, expected }) => {
        expect(stringify(parse(input))).toBe(expected);
      });

      it.each([
        { input: 'let;', desc: 'missing identifier' },
        { input: 'let = 5;', desc: 'missing identifier' },
        { input: 'let x;', desc: 'missing value' },
      ])('should throw for $desc: $input', ({ input }) => {
        expect(() => parse(input)).toThrow();
      });
    });

    describe('index expressions', () => {
      it.each([
        { input: 'arr[0]', expected: '(index arr 0)' },
        { input: 'arr[1]', expected: '(index arr 1)' },
        { input: 'arr[x]', expected: '(index arr x)' },
        { input: 'arr[1 + 2]', expected: '(index arr (+ 1 2))' },
        { input: '[1, 2, 3][0]', expected: '(index (array 1 2 3) 0)' },
        { input: 'arr[0] + arr[1]', expected: '(+ (index arr 0) (index arr 1))' },
      ])('should parse $input to $expected', ({ input, expected }) => {
        expect(stringify(parse(input))).toBe(expected);
      });

      it('should throw for missing closing bracket', () => {
        expect(() => parse('arr[0')).toThrow('Missing closing bracket in index expression');
      });
    });

    describe('index assign statements', () => {
      it.each([
        { input: 'arr[0] = 99', expected: '(index-assign (index arr 0) 99)' },
        { input: 'arr[1] = 42', expected: '(index-assign (index arr 1) 42)' },
        { input: 'arr[0] = "hello"', expected: '(index-assign (index arr 0) "hello")' },
        { input: 'arr[i] = x + 1', expected: '(index-assign (index arr i) (+ x 1))' },
      ])('should parse $input to $expected', ({ input, expected }) => {
        expect(stringify(parse(input))).toBe(expected);
      });

      it('should parse AST node fields for arr[0] = 5', () => {
        const stmt = parse('arr[0] = 5').statements[0] as IndexAssignStatement;
        expect(stmt.kind).toBe(NodeKind.INDEX_ASSIGN_STATEMENT);
        expect(stmt.tokenLiteral()).toBe('arr');
        expect((stmt.left.left as Identifier).value).toBe('arr');
        expect((stmt.left.index as NumberLiteral).value).toBe(0);
        expect((stmt.right as NumberLiteral).value).toBe(5);
      });
    });
  });

  describe('const statements', () => {
    it.each([
      { input: 'const x = 5;', expected: '(const x 5)' },
      { input: 'const result = 2 + 3;', expected: '(const result (+ 2 3))' },
      { input: 'const z = x * y;', expected: '(const z (* x y))' },
      { input: 'const greeting = "hi";', expected: '(const greeting "hi")' },
      { input: 'const a = [1, 2, 3];', expected: '(const a (array 1 2 3))' },
      { input: 'const a = ["x", "y"];', expected: '(const a (array "x" "y"))' },
      { input: 'const a = [];', expected: '(const a (array ))' },
    ])('should parse $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it.each([
      { input: 'const;', desc: 'missing identifier' },
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
      { input: 'x = -5;', expected: '(= x (-5))' },
      { input: 'x = -(5);', expected: '(= x (-5))' },
      { input: 'x = -(-5);', expected: '(= x (-(-5)))' },
      { input: 'x = !5;', expected: '(= x (!5))' },
      { input: 'x = !!5;', expected: '(= x (!!5))' },
      { input: 'y = !true;', expected: '(= y (!true))' },
      { input: 'y = !!true;', expected: '(= y (!!true))' },
      { input: 'z = -a;', expected: '(= z (-a))' },
      { input: 'z = !a;', expected: '(= z (!a))' },
      { input: 'z = !!a;', expected: '(= z (!!a))' },
      { input: 'z = !nil;', expected: '(= z (!nil))' },
      { input: 'z = !!nil;', expected: '(= z (!!nil))' },
      { input: 'z = ![];', expected: '(= z (!(array )))' },
      { input: 'z = !![];', expected: '(= z (!!(array )))' },
      { input: 'z = !arr[i];', expected: '(= z (!(index arr i)))' },
      { input: 'z = !!arr[i];', expected: '(= z (!!(index arr i)))' },
    ])('should parse $input to $expected', ({ input, expected }) => {
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

      { input: '1 < 2', expected: '(< 1 2)' },
      { input: '1 > 2', expected: '(> 1 2)' },
      { input: '1 <= 2', expected: '(<= 1 2)' },
      { input: '1 >= 2', expected: '(>= 1 2)' },
      { input: '1 == 2', expected: '(== 1 2)' },
      { input: '1 != 2', expected: '(!= 1 2)' },

      { input: 'true == true', expected: '(== true true)' },
      { input: 'true != false', expected: '(!= true false)' },

      { input: '"str" == "str"', expected: '(== "str" "str")' },

      { input: 'x < 10 == y > 5', expected: '(== (< x 10) (> y 5))' },
      { input: 'x + y < z', expected: '(< (+ x y) z)' },
      { input: 'x < y + z', expected: '(< x (+ y z))' },
    ])('should parse $input to $expected', ({ input, expected }) => {
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
      { input: '{ let a = [1, 2] }', expected: '(block (let a (array 1 2)))' },
      { input: '{ [1, 2] }', expected: '(block (array 1 2))' },
    ])('should parse $input to $expected', ({ input, expected }) => {
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
    ])('should parse $input to $expected', ({ input, expected }) => {
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
      ])('should parse $input to $expected', ({ input, expected }) => {
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
        {
          input: 'fn items() { return [1, 2, 3] }',
          expected: '(fn items () (block (return (array 1 2 3))))',
        },
      ])('should parse return inside fn body: $input', ({ input, expected }) => {
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
    ])('should parse $input to $expected', ({ input, expected }) => {
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
      { input: 'return [1, 2, 3]', expected: '(return (array 1 2 3))' },
      { input: 'return ["a", "b"]', expected: '(return (array "a" "b"))' },
      { input: 'return []', expected: '(return (array ))' },
    ])('should parse $input to $expected', ({ input, expected }) => {
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

  describe('prefix expressions', () => {
    it.each([
      { input: '-5', expected: '(-5)' },
      { input: '!true', expected: '(!true)' },
      { input: '-a', expected: '(-a)' },
      { input: '!!a', expected: '(!!a)' },
      { input: '!false', expected: '(!false)' },
      { input: '!!true', expected: '(!!true)' },
      { input: '!nil', expected: '(!nil)' },
      { input: '!!"string"', expected: '(!!"string")' },
      { input: '![]', expected: '(!(array ))' },
      { input: '!![1, 2]', expected: '(!!(array 1 2))' },
      { input: '-arr[0]', expected: '(-(index arr 0))' },
      { input: '!arr[i]', expected: '(!(index arr i))' },
      { input: '!!arr[1 + 2]', expected: '(!!(index arr (+ 1 2)))' },
    ])('should parse $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });
  });

  describe('if statements', () => {
    it.each([
      { input: 'if (true) { 10 }', expected: '(if true (block 10))' },
      { input: 'if (x > 10) { x }', expected: '(if (> x 10) (block x))' },
      {
        input: 'if (x > 10) { 10 } else { 20 }',
        expected: '(if (> x 10) (block 10) (block 20))',
      },
      {
        input: 'if (x > 10) { 10 } elif (x > 5) { 5 } else { 0 }',
        expected: '(if (> x 10) (block 10) (if (> x 5) (block 5) (block 0)))',
      },
      {
        input: 'if (x > 10) { 10 } elif (x > 5) { 5 } elif (x > 2) { 2 } else { 0 }',
        expected:
          '(if (> x 10) (block 10) (if (> x 5) (block 5) (if (> x 2) (block 2) (block 0))))',
      },
    ])('should parse $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it.each([
      { input: 'if true { 10 }', desc: 'missing condition parens' },
      { input: 'if (true { 10 }', desc: 'missing condition parens' },
      { input: 'if )true { 10 }', desc: 'missing condition parens' },
      { input: 'if true) { 10 }', desc: 'missing condition parens' },
      { input: 'if (true) 10', desc: 'missing block braces' },
      { input: 'if (true) { 10', desc: 'missing block braces' },
      { input: 'if (true) } 10', desc: 'missing block braces' },
      { input: 'if (true) 10 }', desc: 'missing block braces' },
      { input: 'if (true) { 10 } else 20', desc: 'missing else block braces' },
      { input: 'if (true) { 10 } elif (false) 20', desc: 'missing elif block braces' },
    ])('should throw for $desc: $input', ({ input }) => {
      expect(() => parse(input)).toThrow();
    });
  });

  describe('while statements', () => {
    it.each([
      { input: 'while (true) { 10 }', expected: '(while true (block 10))' },
      { input: 'while (x < 10) { x = x + 1 }', expected: '(while (< x 10) (block (= x (+ x 1))))' },
      {
        input: 'while (i < 5) { sum = sum + i; i = i + 1; }',
        expected: '(while (< i 5) (block (= sum (+ sum i)) (= i (+ i 1))))',
      },
    ])('should parse $input to $expected', ({ input, expected }) => {
      expect(stringify(parse(input))).toBe(expected);
    });

    it.each([
      { input: 'while true { 10 }', desc: 'missing condition parens' },
      { input: 'while (true { 10 }', desc: 'missing condition parens' },
      { input: 'while )true { 10 }', desc: 'missing condition parens' },
      { input: 'while true) { 10 }', desc: 'missing condition parens' },
      { input: 'while (true) 10', desc: 'missing block braces' },
      { input: 'while (true) { 10', desc: 'missing block braces' },
      { input: 'while (true) } 10', desc: 'missing block braces' },
      { input: 'while (true) 10 }', desc: 'missing block braces' },
    ])('should throw for $desc: $input', ({ input }) => {
      expect(() => parse(input)).toThrow();
    });
  });

  describe('stop statements', () => {
    it('should parse stop statement', () => {
      expect(stringify(parse('stop'))).toBe('stop');
    });

    it('should parse stop statement in a while loop', () => {
      const input = 'while (true) { stop }';
      expect(stringify(parse(input))).toBe('(while true (block stop))');
    });

    it('should parse stop statement with semicolon', () => {
      expect(stringify(parse('stop;'))).toBe('stop');
    });

    it('should parse stop statement AST node fields', () => {
      const stmt = parse('stop').statements[0] as BreakStatement;
      expect(stmt.kind).toBe(NodeKind.BREAK_STATEMENT);
      expect(stmt.tokenLiteral()).toBe('stop');
    });
  });
});
