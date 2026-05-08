import type { SourceLocation } from '../errors';
import type { BuiltInOptions } from './builtins';
import type { RuntimeValue } from './interpreter.interface';
import { ScopeError } from '../errors';
import { NodeKind } from '../parser/ast';
import { createBuiltins } from './builtins';
import { IdentifierValue, IdentifierValueMeta } from './values/identifier.value';

export class Environment {
  static createGlobal(options: BuiltInOptions = {}): Environment {
    const env = new Environment();
    const builtins = createBuiltins(options);
    for (const [name, value] of Object.entries(builtins)) {
      env.declare(new IdentifierValue(name, NodeKind.CONST_STATEMENT), value, {
        line: 0,
        column: 0,
        offset: 0,
        length: 0,
      });
    }
    return env;
  }

  private readonly _outer: Environment | undefined;

  private readonly _env: Map<string, [RuntimeValue, IdentifierValueMeta]>;

  constructor(env?: Environment) {
    this._outer = env;
    this._env = new Map();
  }

  assign(variable: IdentifierValue, value: RuntimeValue, location: SourceLocation): void {
    const env = this._resolve(variable, location);
    env._set(variable, value);
  }

  declare(variable: IdentifierValue, value: RuntimeValue, location: SourceLocation): void {
    if (this._contains(variable)) {
      throw new ScopeError(`redeclaration of ${variable.value}`, location);
    }

    this._set(variable, value);
  }

  checkMeta(varialbe: IdentifierValue, location: SourceLocation): IdentifierValueMeta | null {
    try {
      const env = this._resolve(varialbe, location);
      return env._getMeta(varialbe);
    } catch {
      return null;
    }
  }

  lookup(varialbe: IdentifierValue, location: SourceLocation): RuntimeValue {
    const env = this._resolve(varialbe, location);
    return env._get(varialbe);
  }

  private _resolve(variable: IdentifierValue, location: SourceLocation): Environment {
    if (this._contains(variable)) return this;

    if (this._outer) return this._outer._resolve(variable, location);

    throw new ScopeError(`${variable.value} was not declared in this scope`, location);
  }

  private _get(variable: IdentifierValue): RuntimeValue {
    const [value, _] = this._env.get(this._toKey(variable))!;
    return value;
  }

  private _getMeta(variable: IdentifierValue): IdentifierValueMeta {
    const [_, meta] = this._env.get(this._toKey(variable))!;
    return meta;
  }

  private _set(variable: IdentifierValue, value: RuntimeValue): void {
    this._env.set(this._toKey(variable), [value, new IdentifierValueMeta(variable.kind)]);
  }

  private _contains(variable: IdentifierValue): boolean {
    return this._env.has(this._toKey(variable));
  }

  private _toKey(variable: IdentifierValue): string {
    return variable.value;
  }
}
