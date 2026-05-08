import { Lexer } from '../lexer/lexer';
import { Parser } from '../parser/parser';
import { ErrorType } from './error.interface';
import { ErrorReporter } from './error-reporter';
import { InterpreterError } from './errors';

describe('Error Reporting', () => {
  describe('Lexical Errors', () => {
    it('should report unclosed string literal with correct location', () => {
      const source = 'let x = "hello';
      const lexer = new Lexer(source);

      try {
        lexer.tokenize();
        fail('Should have thrown an InterpreterError');
      } catch (e) {
        const error = e as InterpreterError;
        expect(error.type).toBe(ErrorType.LEXICAL);
        expect(error.location).toMatchObject({
          line: 1,
          column: 15,
          length: 1,
        });

        const output = ErrorReporter.report(error, source);
        expect(output).toContain('Lexical Error: Unclosed double quotes');
        expect(output).toContain('1 | let x = "hello');
        expect(output).toContain('              ^');
      }
    });
  });

  describe('Syntax Errors', () => {
    it('should report missing semicolon or expected token', () => {
      const source = 'let x = 5\nlet y = 10';
      const lexer = new Lexer(source);
      const parser = new Parser(lexer.tokenize());

      try {
        parser.parse();
      } catch (e) {
        const error = e as InterpreterError;
        expect(error.type).toBe(ErrorType.PARSE);

        const output = ErrorReporter.report(error, source);
        expect(output).toContain('Syntax Error');
      }
    });

    it('should report unexpected token in expression', () => {
      const source = 'let x = 5 + * 10;';
      const lexer = new Lexer(source);
      const parser = new Parser(lexer.tokenize());

      try {
        parser.parse();
        fail('Should have thrown an InterpreterError');
      } catch (e) {
        const error = e as InterpreterError;
        expect(error.type).toBe(ErrorType.PARSE);
        expect(error.location.line).toBe(1);
        expect(error.location.column).toBe(13);

        const output = ErrorReporter.report(error, source);
        expect(output).toContain('1 | let x = 5 + * 10;');
        expect(output).toContain('                ^');
      }
    });
  });

  describe('ErrorReporter', () => {
    it('should format a multiline error correctly', () => {
      const source = 'fn hello() {\n  return 5 +\n}';
      const error = new InterpreterError(ErrorType.PARSE, 'Expected expression', {
        line: 3,
        column: 1,
        offset: 24,
        length: 1,
      });

      const output = ErrorReporter.report(error, source);
      expect(output).toContain('3 | }');
      expect(output).toContain('    ^');
    });
  });
});
