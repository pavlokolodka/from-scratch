import { Lexer } from '../lexer/lexer';
import { Parser } from '../parser/parser';
import { Environment } from './environment';
import { Interpreter } from './interpreter';
import { RuntimeType } from './interpreter.interface';

describe('Built-ins', () => {
  let interpreter: Interpreter;

  beforeEach(() => {
    interpreter = new Interpreter();
  });

  function evaluate(input: string) {
    const lexer = new Lexer(input);
    const parser = new Parser(lexer.tokenize());
    const env = Environment.createGlobal();
    const program = parser.parse();

    const stmt = program.statements[0];

    return interpreter.eval(stmt, env);
  }

  describe('print', () => {
    let logSpy: jest.SpyInstance;

    beforeEach(() => {
      logSpy = jest.spyOn(console, 'log').mockImplementation(() => {});
    });

    afterEach(() => {
      logSpy.mockRestore();
    });

    it('should print a number', () => {
      evaluate('print(5)');
      expect(logSpy).toHaveBeenCalledWith('5');
    });

    it('should print a string', () => {
      evaluate('print("hello")');
      expect(logSpy).toHaveBeenCalledWith('hello');
    });

    it('should print multiple values', () => {
      evaluate('print(1, "two", true)');
      expect(logSpy).toHaveBeenCalledWith('1', 'two', 'true');
    });

    it('should print an array', () => {
      evaluate('print([1, 2, 3])');
      expect(logSpy).toHaveBeenCalledWith('[1, 2, 3]');
    });

    it('should print nil', () => {
      evaluate('print(nil)');
      expect(logSpy).toHaveBeenCalledWith('nil');
    });

    it('should throw an error when redeclaring a print', () => {
      expect(() => evaluate('let print = 1;')).toThrow();
      expect(logSpy).not.toHaveBeenCalled();
    });

    it('should return void', () => {
      const result = evaluate('print(5)');
      expect(result).toEqual(expect.objectContaining({ type: RuntimeType.VOID }));
    });
  });
});
