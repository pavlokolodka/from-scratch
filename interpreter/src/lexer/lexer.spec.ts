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
