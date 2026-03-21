import { Environment } from './environment';
import { NumberValue } from './values/number.value';

describe('Environment', () => {
  let env: Environment;

  beforeEach(() => {
    env = new Environment();
  });

  describe('declare', () => {
    it('should declare a variable', () => {
      env.declare('x', new NumberValue(10));
      expect(env.lookup('x')).toEqual(new NumberValue(10));
    });

    it.each([
      { name: 'x', value: new NumberValue(1) },
      { name: 'count', value: new NumberValue(100) },
      { name: 'result', value: new NumberValue(0) },
    ])('should declare variable $name with value $value.value', ({ name, value }) => {
      env.declare(name, value);
      expect(env.lookup(name)).toEqual(value);
    });
  });

  describe('lookup', () => {
    it('should find a variable declared in the current scope', () => {
      env.declare('y', new NumberValue(7));
      expect(env.lookup('y')).toEqual(new NumberValue(7));
    });

    it('should find a variable declared in an outer scope', () => {
      env.declare('outer', new NumberValue(99));
      const inner = new Environment(env);
      expect(inner.lookup('outer')).toEqual(new NumberValue(99));
    });

    it('should find a variable through multiple scope levels', () => {
      env.declare('deep', new NumberValue(5));
      const middle = new Environment(env);
      const inner = new Environment(middle);
      expect(inner.lookup('deep')).toEqual(new NumberValue(5));
    });

    it('should shadow an outer variable with an inner declaration', () => {
      env.declare('x', new NumberValue(1));
      const inner = new Environment(env);
      inner.declare('x', new NumberValue(2));
      expect(inner.lookup('x')).toEqual(new NumberValue(2));
      expect(env.lookup('x')).toEqual(new NumberValue(1));
    });

    it('should throw when the variable is not declared', () => {
      expect(() => env.lookup('undefined_var')).toThrow();
    });

    it('should throw when variable is not declared in any scope', () => {
      const inner = new Environment(env);
      expect(() => inner.lookup('ghost')).toThrow();
    });
  });

  describe('assign', () => {
    it('should update a variable in the current scope and return the new value', () => {
      env.declare('x', new NumberValue(1));
      const result = new NumberValue(2);
      env.assign('x', new NumberValue(2));
      expect(env.lookup('x')).toEqual(result);
    });

    it('should update a variable declared in an outer scope', () => {
      env.declare('x', new NumberValue(10));
      const inner = new Environment(env);
      inner.assign('x', new NumberValue(20));
      expect(env.lookup('x')).toEqual(new NumberValue(20));
    });

    it('should update the nearest scope when the variable is shadowed', () => {
      env.declare('x', new NumberValue(1));
      const inner = new Environment(env);
      inner.declare('x', new NumberValue(2));
      inner.assign('x', new NumberValue(3));
      expect(inner.lookup('x')).toEqual(new NumberValue(3));
      expect(env.lookup('x')).toEqual(new NumberValue(1));
    });

    it('should throw when assigning to an undeclared variable', () => {
      expect(() => env.assign('undeclared', new NumberValue(1))).toThrow();
    });
  });

  describe('scope isolation', () => {
    it('should not expose inner scope variables to the outer scope', () => {
      const inner = new Environment(env);
      inner.declare('secret', new NumberValue(42));
      expect(() => env.lookup('secret')).toThrow();
    });

    it('should allow sibling scopes to have the same variable name independently', () => {
      const scope1 = new Environment(env);
      const scope2 = new Environment(env);
      scope1.declare('x', new NumberValue(1));
      scope2.declare('x', new NumberValue(2));
      expect(scope1.lookup('x')).toEqual(new NumberValue(1));
      expect(scope2.lookup('x')).toEqual(new NumberValue(2));
    });
  });
});
