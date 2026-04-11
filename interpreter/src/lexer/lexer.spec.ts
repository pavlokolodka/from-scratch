import { Lexer } from './lexer';
import { TokenType } from './lexer.interface';

describe('Lexer', () => {
  describe('essential tokens', () => {
    const input = `= + ( ) { } [ ] , : ; < > let const if elif else while fn nil five 5 "foobar"`;
    const tokens = new Lexer(input).tokenize();
    const expected = [
      { type: TokenType.ASSIGN, literal: '=', line: 1 },
      { type: TokenType.PLUS, literal: '+', line: 1 },
      { type: TokenType.LPAREN, literal: '(', line: 1 },
      { type: TokenType.RPAREN, literal: ')', line: 1 },
      { type: TokenType.LBRACE, literal: '{', line: 1 },
      { type: TokenType.RBRACE, literal: '}', line: 1 },
      { type: TokenType.LBRACKET, literal: '[', line: 1 },
      { type: TokenType.RBRACKET, literal: ']', line: 1 },
      { type: TokenType.COMMA, literal: ',', line: 1 },
      { type: TokenType.COLON, literal: ':', line: 1 },
      { type: TokenType.SEMICOLON, literal: ';', line: 1 },
      { type: TokenType.LT, literal: '<', line: 1 },
      { type: TokenType.GT, literal: '>', line: 1 },
      { type: TokenType.LET, literal: 'let', line: 1 },
      { type: TokenType.CONST, literal: 'const', line: 1 },
      { type: TokenType.IF, literal: 'if', line: 1 },
      { type: TokenType.ELIF, literal: 'elif', line: 1 },
      { type: TokenType.ELSE, literal: 'else', line: 1 },
      { type: TokenType.WHILE, literal: 'while', line: 1 },
      { type: TokenType.FUNCTION, literal: 'fn', line: 1 },
      { type: TokenType.NIL, literal: 'nil', line: 1 },
      { type: TokenType.IDENT, literal: 'five', line: 1 },
      { type: TokenType.NUMBER, literal: '5', line: 1 },
      { type: TokenType.STRING, literal: 'foobar', line: 1 },
      { type: TokenType.EOF, literal: '', line: 1 },
    ];

    it('should have correct number of tokens', () => {
      expect(tokens.length).toBe(expected.length);
    });

    it.each(expected.map((e, i) => ({ ...e, i })))('token $i should be $literal', ({
      type,
      literal,
      line,
      i,
    }) => {
      expect(tokens[i].type).toBe(type);
      expect(tokens[i].literal).toBe(literal);
      expect(tokens[i].line).toBe(line);
    });
  });

  describe('let keyword', () => {
    it('should tokenize let declaration', () => {
      const input = `let x = 5;`;
      const tests = [
        { type: TokenType.LET, literal: 'let', line: 1 },
        { type: TokenType.IDENT, literal: 'x', line: 1 },
        { type: TokenType.ASSIGN, literal: '=', line: 1 },
        { type: TokenType.NUMBER, literal: '5', line: 1 },
        { type: TokenType.SEMICOLON, literal: ';', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const l = new Lexer(input);
      const tokens = l.tokenize();

      expect(tokens.length).toBe(tests.length);

      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize let reassignment', () => {
      const input = ['let x = 5;', 'x = 10;'].join('\n');
      const tests = [
        { type: TokenType.LET, literal: 'let', line: 1 },
        { type: TokenType.IDENT, literal: 'x', line: 1 },
        { type: TokenType.ASSIGN, literal: '=', line: 1 },
        { type: TokenType.NUMBER, literal: '5', line: 1 },
        { type: TokenType.SEMICOLON, literal: ';', line: 1 },
        { type: TokenType.IDENT, literal: 'x', line: 2 },
        { type: TokenType.ASSIGN, literal: '=', line: 2 },
        { type: TokenType.NUMBER, literal: '10', line: 2 },
        { type: TokenType.SEMICOLON, literal: ';', line: 2 },
        { type: TokenType.EOF, literal: '', line: 2 },
      ];

      const l = new Lexer(input);
      const tokens = l.tokenize();

      expect(tokens.length).toBe(tests.length);

      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });
  });

  describe('const keyword', () => {
    it('should tokenize const declaration', () => {
      const input = `const y = 10;`;
      const tests = [
        { type: TokenType.CONST, literal: 'const', line: 1 },
        { type: TokenType.IDENT, literal: 'y', line: 1 },
        { type: TokenType.ASSIGN, literal: '=', line: 1 },
        { type: TokenType.NUMBER, literal: '10', line: 1 },
        { type: TokenType.SEMICOLON, literal: ';', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const l = new Lexer(input);
      const tokens = l.tokenize();

      expect(tokens.length).toBe(tests.length);

      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize const reassignment', () => {
      const input = ['const y = 10;', 'y = 20;'].join('\n');
      const tests = [
        { type: TokenType.CONST, literal: 'const', line: 1 },
        { type: TokenType.IDENT, literal: 'y', line: 1 },
        { type: TokenType.ASSIGN, literal: '=', line: 1 },
        { type: TokenType.NUMBER, literal: '10', line: 1 },
        { type: TokenType.SEMICOLON, literal: ';', line: 1 },
        { type: TokenType.IDENT, literal: 'y', line: 2 },
        { type: TokenType.ASSIGN, literal: '=', line: 2 },
        { type: TokenType.NUMBER, literal: '20', line: 2 },
        { type: TokenType.SEMICOLON, literal: ';', line: 2 },
        { type: TokenType.EOF, literal: '', line: 2 },
      ];

      const l = new Lexer(input);
      const tokens = l.tokenize();

      expect(tokens.length).toBe(tests.length);

      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });
  });

  describe('block', () => {
    it('should tokenize an empty block', () => {
      const input = `{}`;
      const tests = [
        { type: TokenType.LBRACE, literal: '{', line: 1 },
        { type: TokenType.RBRACE, literal: '}', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize a block with statements', () => {
      const input = `{\n  let x = 5\n  x\n}`;
      const tests = [
        { type: TokenType.LBRACE, literal: '{', line: 1 },
        { type: TokenType.LET, literal: 'let', line: 2 },
        { type: TokenType.IDENT, literal: 'x', line: 2 },
        { type: TokenType.ASSIGN, literal: '=', line: 2 },
        { type: TokenType.NUMBER, literal: '5', line: 2 },
        { type: TokenType.IDENT, literal: 'x', line: 3 },
        { type: TokenType.RBRACE, literal: '}', line: 4 },
        { type: TokenType.EOF, literal: '', line: 4 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });
  });

  describe('fn keyword', () => {
    it('should tokenize a function with no parameters', () => {
      const input = `fn greet() {}`;
      const tests = [
        { type: TokenType.FUNCTION, literal: 'fn', line: 1 },
        { type: TokenType.IDENT, literal: 'greet', line: 1 },
        { type: TokenType.LPAREN, literal: '(', line: 1 },
        { type: TokenType.RPAREN, literal: ')', line: 1 },
        { type: TokenType.LBRACE, literal: '{', line: 1 },
        { type: TokenType.RBRACE, literal: '}', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize a function with parameters', () => {
      const input = `fn add(a, b) { a + b }`;
      const tests = [
        { type: TokenType.FUNCTION, literal: 'fn', line: 1 },
        { type: TokenType.IDENT, literal: 'add', line: 1 },
        { type: TokenType.LPAREN, literal: '(', line: 1 },
        { type: TokenType.IDENT, literal: 'a', line: 1 },
        { type: TokenType.COMMA, literal: ',', line: 1 },
        { type: TokenType.IDENT, literal: 'b', line: 1 },
        { type: TokenType.RPAREN, literal: ')', line: 1 },
        { type: TokenType.LBRACE, literal: '{', line: 1 },
        { type: TokenType.IDENT, literal: 'a', line: 1 },
        { type: TokenType.PLUS, literal: '+', line: 1 },
        { type: TokenType.IDENT, literal: 'b', line: 1 },
        { type: TokenType.RBRACE, literal: '}', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize a multiline function declaration', () => {
      const input = `fn add(a, b) {\n  a + b\n}`;
      const tests = [
        { type: TokenType.FUNCTION, literal: 'fn', line: 1 },
        { type: TokenType.IDENT, literal: 'add', line: 1 },
        { type: TokenType.LPAREN, literal: '(', line: 1 },
        { type: TokenType.IDENT, literal: 'a', line: 1 },
        { type: TokenType.COMMA, literal: ',', line: 1 },
        { type: TokenType.IDENT, literal: 'b', line: 1 },
        { type: TokenType.RPAREN, literal: ')', line: 1 },
        { type: TokenType.LBRACE, literal: '{', line: 1 },
        { type: TokenType.IDENT, literal: 'a', line: 2 },
        { type: TokenType.PLUS, literal: '+', line: 2 },
        { type: TokenType.IDENT, literal: 'b', line: 2 },
        { type: TokenType.RBRACE, literal: '}', line: 3 },
        { type: TokenType.EOF, literal: '', line: 3 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });
  });

  describe('boolean literals', () => {
    it('should tokenize true and false', () => {
      const input = `true false`;
      const tests = [
        { type: TokenType.TRUE, literal: 'true', line: 1 },
        { type: TokenType.FALSE, literal: 'false', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const l = new Lexer(input);
      const tokens = l.tokenize();

      expect(tokens.length).toBe(tests.length);

      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });
  });

  describe('fn invocation', () => {
    it('should tokenize a call with no arguments', () => {
      const input = `greet()`;
      const tests = [
        { type: TokenType.IDENT, literal: 'greet', line: 1 },
        { type: TokenType.LPAREN, literal: '(', line: 1 },
        { type: TokenType.RPAREN, literal: ')', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize a call with one argument', () => {
      const input = `double(5)`;
      const tests = [
        { type: TokenType.IDENT, literal: 'double', line: 1 },
        { type: TokenType.LPAREN, literal: '(', line: 1 },
        { type: TokenType.NUMBER, literal: '5', line: 1 },
        { type: TokenType.RPAREN, literal: ')', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize a call with multiple arguments', () => {
      const input = `add(a, b)`;
      const tests = [
        { type: TokenType.IDENT, literal: 'add', line: 1 },
        { type: TokenType.LPAREN, literal: '(', line: 1 },
        { type: TokenType.IDENT, literal: 'a', line: 1 },
        { type: TokenType.COMMA, literal: ',', line: 1 },
        { type: TokenType.IDENT, literal: 'b', line: 1 },
        { type: TokenType.RPAREN, literal: ')', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize a call with expression arguments', () => {
      const input = `add(1 + 2, 3 * 4)`;
      const tests = [
        { type: TokenType.IDENT, literal: 'add', line: 1 },
        { type: TokenType.LPAREN, literal: '(', line: 1 },
        { type: TokenType.NUMBER, literal: '1', line: 1 },
        { type: TokenType.PLUS, literal: '+', line: 1 },
        { type: TokenType.NUMBER, literal: '2', line: 1 },
        { type: TokenType.COMMA, literal: ',', line: 1 },
        { type: TokenType.NUMBER, literal: '3', line: 1 },
        { type: TokenType.MULTIPLY, literal: '*', line: 1 },
        { type: TokenType.NUMBER, literal: '4', line: 1 },
        { type: TokenType.RPAREN, literal: ')', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });
  });

  describe('return keyword', () => {
    it('should tokenize a return statement', () => {
      const input = `return 5`;
      const tests = [
        { type: TokenType.RETURN, literal: 'return', line: 1 },
        { type: TokenType.NUMBER, literal: '5', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize return with expression', () => {
      const input = `return a + b`;
      const tests = [
        { type: TokenType.RETURN, literal: 'return', line: 1 },
        { type: TokenType.IDENT, literal: 'a', line: 1 },
        { type: TokenType.PLUS, literal: '+', line: 1 },
        { type: TokenType.IDENT, literal: 'b', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize return inside a function', () => {
      const input = `fn double(x) { return x * 2 }`;
      const tests = [
        { type: TokenType.FUNCTION, literal: 'fn', line: 1 },
        { type: TokenType.IDENT, literal: 'double', line: 1 },
        { type: TokenType.LPAREN, literal: '(', line: 1 },
        { type: TokenType.IDENT, literal: 'x', line: 1 },
        { type: TokenType.RPAREN, literal: ')', line: 1 },
        { type: TokenType.LBRACE, literal: '{', line: 1 },
        { type: TokenType.RETURN, literal: 'return', line: 1 },
        { type: TokenType.IDENT, literal: 'x', line: 1 },
        { type: TokenType.MULTIPLY, literal: '*', line: 1 },
        { type: TokenType.NUMBER, literal: '2', line: 1 },
        { type: TokenType.RBRACE, literal: '}', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });
  });

  describe('string literals', () => {
    it('should tokenize a simple string literal', () => {
      const input = `"hello"`;
      const tests = [
        { type: TokenType.STRING, literal: 'hello', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize an empty string literal', () => {
      const input = `""`;
      const tests = [
        { type: TokenType.STRING, literal: '', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize a string with spaces', () => {
      const input = `"hello world"`;
      const tests = [
        { type: TokenType.STRING, literal: 'hello world', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize a string assigned to a variable', () => {
      const input = `let name = "alice";`;
      const tests = [
        { type: TokenType.LET, literal: 'let', line: 1 },
        { type: TokenType.IDENT, literal: 'name', line: 1 },
        { type: TokenType.ASSIGN, literal: '=', line: 1 },
        { type: TokenType.STRING, literal: 'alice', line: 1 },
        { type: TokenType.SEMICOLON, literal: ';', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize multiple strings in one line', () => {
      const input = `"foo" "bar"`;
      const tests = [
        { type: TokenType.STRING, literal: 'foo', line: 1 },
        { type: TokenType.STRING, literal: 'bar', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize a string passed as a function argument', () => {
      const input = `greet("world")`;
      const tests = [
        { type: TokenType.IDENT, literal: 'greet', line: 1 },
        { type: TokenType.LPAREN, literal: '(', line: 1 },
        { type: TokenType.STRING, literal: 'world', line: 1 },
        { type: TokenType.RPAREN, literal: ')', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should produce ILLEGAL tokens for single-quoted strings', () => {
      const input = `'hello'`;
      const tests = [
        { type: TokenType.ILLEGAL, literal: "'", line: 1 },
        { type: TokenType.IDENT, literal: 'hello', line: 1 },
        { type: TokenType.ILLEGAL, literal: "'", line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should produce a single ILLEGAL token for a lone single quote', () => {
      const input = `'`;
      const tests = [
        { type: TokenType.ILLEGAL, literal: "'", line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should throw for an unclosed string literal', () => {
      expect(() => new Lexer('"hello').tokenize()).toThrow();
    });

    it('should throw for a string with only an opening quote', () => {
      expect(() => new Lexer('"').tokenize()).toThrow();
    });

    it('should throw for an unclosed string in a let declaration', () => {
      expect(() => new Lexer('let x = "hello').tokenize()).toThrow();
    });
  });

  describe('array index access', () => {
    it('should tokenize simple index access arr[0]', () => {
      const input = `arr[0]`;
      const tests = [
        { type: TokenType.IDENT, literal: 'arr', line: 1 },
        { type: TokenType.LBRACKET, literal: '[', line: 1 },
        { type: TokenType.NUMBER, literal: '0', line: 1 },
        { type: TokenType.RBRACKET, literal: ']', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize index access with expression index arr[1 + 2]', () => {
      const input = `arr[1 + 2]`;
      const tests = [
        { type: TokenType.IDENT, literal: 'arr', line: 1 },
        { type: TokenType.LBRACKET, literal: '[', line: 1 },
        { type: TokenType.NUMBER, literal: '1', line: 1 },
        { type: TokenType.PLUS, literal: '+', line: 1 },
        { type: TokenType.NUMBER, literal: '2', line: 1 },
        { type: TokenType.RBRACKET, literal: ']', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });

    it('should tokenize let with index access let x = arr[0]', () => {
      const input = `let x = arr[0];`;
      const tests = [
        { type: TokenType.LET, literal: 'let', line: 1 },
        { type: TokenType.IDENT, literal: 'x', line: 1 },
        { type: TokenType.ASSIGN, literal: '=', line: 1 },
        { type: TokenType.IDENT, literal: 'arr', line: 1 },
        { type: TokenType.LBRACKET, literal: '[', line: 1 },
        { type: TokenType.NUMBER, literal: '0', line: 1 },
        { type: TokenType.RBRACKET, literal: ']', line: 1 },
        { type: TokenType.SEMICOLON, literal: ';', line: 1 },
        { type: TokenType.EOF, literal: '', line: 1 },
      ];

      const tokens = new Lexer(input).tokenize();

      expect(tokens.length).toBe(tests.length);
      for (let i = 0; i < tests.length; i++) {
        expect(tokens[i].type).toBe(tests[i].type);
        expect(tokens[i].literal).toBe(tests[i].literal);
        expect(tokens[i].line).toBe(tests[i].line);
      }
    });
  });

  it('should tokenize illegal tokens', () => {
    const input = `@ # $`;
    const tests = [
      { type: TokenType.ILLEGAL, literal: '@', line: 1 },
      { type: TokenType.ILLEGAL, literal: '#', line: 1 },
      { type: TokenType.ILLEGAL, literal: '$', line: 1 },
      { type: TokenType.EOF, literal: '', line: 1 },
    ];

    const l = new Lexer(input);
    const tokens = l.tokenize();

    expect(tokens.length).toBe(tests.length);

    for (let i = 0; i < tests.length; i++) {
      expect(tokens[i].type).toBe(tests[i].type);
      expect(tokens[i].literal).toBe(tests[i].literal);
      expect(tokens[i].line).toBe(tests[i].line);
    }
  });
});
