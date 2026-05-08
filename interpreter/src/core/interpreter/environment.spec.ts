import type { SourceLocation } from '../errors';
import { Environment } from './environment';
import { IdentifierValueInternal } from './values/identifier.value';
import { NumberValue } from './values/number.value';

const id = (name: string) => new IdentifierValueInternal(name);
const loc: SourceLocation = { line: 1, column: 1, offset: 0, length: 0 };

describe('Environment', () => {
  let env: Environment;

  beforeEach(() => {
    env = new Environment();
  });

  describe('declare', () => {
    it('should declare a variable', () => {
      env.declare(id('x'), new NumberValue(10), loc);
      expect(env.lookup(id('x'), loc)).toEqual(new NumberValue(10));
    });

    it.each([
      { name: 'x', value: new NumberValue(1) },
      { name: 'count', value: new NumberValue(100) },
      { name: 'result', value: new NumberValue(0) },
    ])('should declare variable $name with value $value.value', ({ name, value }) => {
      env.declare(id(name), value, loc);
      expect(env.lookup(id(name), loc)).toEqual(value);
    });
  });

  describe('lookup', () => {
    it('should find a variable declared in the current scope', () => {
      env.declare(id('y'), new NumberValue(7), loc);
      expect(env.lookup(id('y'), loc)).toEqual(new NumberValue(7));
    });

    it('should find a variable declared in an outer scope', () => {
      env.declare(id('outer'), new NumberValue(99), loc);
      const inner = new Environment(env);
      expect(inner.lookup(id('outer'), loc)).toEqual(new NumberValue(99));
    });

    it('should find a variable through multiple scope levels', () => {
      env.declare(id('deep'), new NumberValue(5), loc);
      const middle = new Environment(env);
      const inner = new Environment(middle);
      expect(inner.lookup(id('deep'), loc)).toEqual(new NumberValue(5));
    });

    it('should shadow an outer variable with an inner declaration', () => {
      env.declare(id('x'), new NumberValue(1), loc);
      const inner = new Environment(env);
      inner.declare(id('x'), new NumberValue(2), loc);
      expect(inner.lookup(id('x'), loc)).toEqual(new NumberValue(2));
      expect(env.lookup(id('x'), loc)).toEqual(new NumberValue(1));
    });

    it('should throw when the variable is not declared', () => {
      expect(() => env.lookup(id('undefined_var'), loc)).toThrow();
    });

    it('should throw when variable is not declared in any scope', () => {
      const inner = new Environment(env);
      expect(() => inner.lookup(id('ghost'), loc)).toThrow();
    });
  });

  describe('assign', () => {
    it('should update a variable in the current scope and return the new value', () => {
      env.declare(id('x'), new NumberValue(1), loc);
      const result = new NumberValue(2);
      env.assign(id('x'), new NumberValue(2), loc);
      expect(env.lookup(id('x'), loc)).toEqual(result);
    });

    it('should update a variable declared in an outer scope', () => {
      env.declare(id('x'), new NumberValue(10), loc);
      const inner = new Environment(env);
      inner.assign(id('x'), new NumberValue(20), loc);
      expect(env.lookup(id('x'), loc)).toEqual(new NumberValue(20));
    });

    it('should update the nearest scope when the variable is shadowed', () => {
      env.declare(id('x'), new NumberValue(1), loc);
      const inner = new Environment(env);
      inner.declare(id('x'), new NumberValue(2), loc);
      inner.assign(id('x'), new NumberValue(3), loc);
      expect(inner.lookup(id('x'), loc)).toEqual(new NumberValue(3));
      expect(env.lookup(id('x'), loc)).toEqual(new NumberValue(1));
    });

    it('should throw when assigning to an undeclared variable', () => {
      expect(() => env.assign(id('undeclared'), new NumberValue(1), loc)).toThrow();
    });
  });

  describe('scope isolation', () => {
    it('should not expose inner scope variables to the outer scope', () => {
      const inner = new Environment(env);
      inner.declare(id('secret'), new NumberValue(42), loc);
      expect(() => env.lookup(id('secret'), loc)).toThrow();
    });

    it('should allow sibling scopes to have the same variable name independently', () => {
      const scope1 = new Environment(env);
      const scope2 = new Environment(env);
      scope1.declare(id('x'), new NumberValue(1), loc);
      scope2.declare(id('x'), new NumberValue(2), loc);
      expect(scope1.lookup(id('x'), loc)).toEqual(new NumberValue(1));
      expect(scope2.lookup(id('x'), loc)).toEqual(new NumberValue(2));
    });
  });
});
