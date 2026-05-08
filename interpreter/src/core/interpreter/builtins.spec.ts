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

    let result: any;
    for (const stmt of program.statements) {
      result = interpreter.eval(stmt, env);
    }
    return result;
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

  describe('len', () => {
    it('should return length of a string', () => {
      const result = evaluate('len("hello")');
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: 5 });
    });

    it('should return length of an empty string', () => {
      const result = evaluate('len("")');
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: 0 });
    });

    it('should return length of an array', () => {
      const result = evaluate('len([1, 2, 3])');
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: 3 });
    });

    it('should return length of an empty array', () => {
      const result = evaluate('len([])');
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: 0 });
    });

    it('should throw for unsupported type', () => {
      expect(() => evaluate('len(5)')).toThrow('Argument to len() not supported, got NUMBER');
    });

    it('should throw for incorrect number of arguments', () => {
      expect(() => evaluate('len("a", "b")')).toThrow('Expected 1 argument for len(), got 2');
    });
  });

  describe('str', () => {
    it.each([
      { input: 'str(5)', expected: '5' },
      { input: 'str(true)', expected: 'true' },
      { input: 'str(false)', expected: 'false' },
      { input: 'str(nil)', expected: 'nil' },
      { input: 'str("hello")', expected: 'hello' },
      { input: 'str([1, 2])', expected: '[1, 2]' },
    ])('should convert $input to string "$expected"', ({ input, expected }) => {
      const result = evaluate(input);
      expect(result).toEqual({ type: RuntimeType.STRING, value: expected });
    });

    it('should throw for incorrect number of arguments', () => {
      expect(() => evaluate('str(1, 2)')).toThrow('Expected 1 argument for str(), got 2');
    });
  });

  describe('type', () => {
    it.each([
      { input: 'type(5)', expected: 'NUMBER' },
      { input: 'type("hi")', expected: 'STRING' },
      { input: 'type(true)', expected: 'BOOLEAN' },
      { input: 'type(nil)', expected: 'NULL' },
      { input: 'type([])', expected: 'ARRAY' },
      { input: 'fn f() {}; type(f)', expected: 'FUNCTION' },
      { input: 'type(print)', expected: 'BUILTIN_FN' },
    ])('should return type of $input as $expected', ({ input, expected }) => {
      const result = evaluate(input);
      expect(result).toEqual({ type: RuntimeType.STRING, value: expected });
    });

    it('should throw for incorrect number of arguments', () => {
      expect(() => evaluate('type(1, 2)')).toThrow('Expected 1 argument for type(), got 2');
    });
  });

  describe('push', () => {
    it('should push an element to an array', () => {
      const input = `
        let a = [1, 2];
        let newLen = push(a, 3);
        a
      `;
      const result = evaluate(input);
      expect(result).toEqual({
        type: RuntimeType.ARRAY,
        value: [
          { type: RuntimeType.NUMBER, value: 1 },
          { type: RuntimeType.NUMBER, value: 2 },
          { type: RuntimeType.NUMBER, value: 3 },
        ],
      });
    });

    it('should push a character to a string', () => {
      const input = `
        let s = "hi";
        let newLen = push(s, "!");
        s
      `;
      const result = evaluate(input);
      expect(result).toEqual({ type: RuntimeType.STRING, value: 'hi!' });
    });

    it('should return new length after push', () => {
      expect(evaluate('let a = [1]; push(a, 2)')).toEqual({ type: RuntimeType.NUMBER, value: 2 });
      expect(evaluate('let s = "a"; push(s, "b")')).toEqual({ type: RuntimeType.NUMBER, value: 2 });
    });

    it('should push multiple times', () => {
      const input = `
        let a = [];
        push(a, 1);
        push(a, 2);
        a
      `;
      const result = evaluate(input);
      expect(result.value).toHaveLength(2);
    });

    it('should throw for unsupported type', () => {
      expect(() => evaluate('push(5, 1)')).toThrow('push() not supported for type NUMBER');
    });

    it('should throw for incorrect number of arguments', () => {
      expect(() => evaluate('push([])')).toThrow('Expected 2 arguments for push(), got 1');
    });
  });

  describe('num', () => {
    it.each([
      { input: 'num("123")', expected: 123 },
      { input: 'num("12.3")', expected: 12.3 },
      { input: 'num(true)', expected: 1 },
      { input: 'num(false)', expected: 0 },
      { input: 'num(5)', expected: 5 },
    ])('should convert $input to number $expected', ({ input, expected }) => {
      const result = evaluate(input);
      expect(result).toEqual({ type: RuntimeType.NUMBER, value: expected });
    });

    it('should throw for invalid string', () => {
      expect(() => evaluate('num("abc")')).toThrow('Could not convert string "abc" to number');
    });

    it('should throw for unsupported type', () => {
      expect(() => evaluate('num([])')).toThrow('num() not supported for type ARRAY');
    });

    it('should throw for incorrect number of arguments', () => {
      expect(() => evaluate('num("1", "2")')).toThrow('Expected 1 argument for num(), got 2');
    });
  });
});
