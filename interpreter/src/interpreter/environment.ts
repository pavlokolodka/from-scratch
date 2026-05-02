import type { RuntimeValue } from './interpreter.interface';
import { NodeKind } from '../parser/ast';
import { builtins } from './builtins';
import { IdentifierValue, IdentifierValueMeta } from './values/identifier.value';

export class Environment {
  static createGlobal(): Environment {
    const env = new Environment();
    for (const [name, value] of Object.entries(builtins)) {
      env.declare(new IdentifierValue(name, NodeKind.CONST_STATEMENT), value);
    }
    return env;
  }

  private readonly _outer: Environment | undefined;

  private readonly _env: Map<string, [RuntimeValue, IdentifierValueMeta]>;

  constructor(env?: Environment) {
    this._outer = env;
    this._env = new Map();
  }

  assign(variable: IdentifierValue, value: RuntimeValue): void {
    const env = this._resolve(variable);
    env._set(variable, value);
  }

  declare(variable: IdentifierValue, value: RuntimeValue): void {
    if (this._contains(variable)) {
      throw new Error(`Error: redeclaration of ${variable}`);
    }

    this._set(variable, value);
  }

  checkMeta(varialbe: IdentifierValue): IdentifierValueMeta | null {
    try {
      const env = this._resolve(varialbe);
      return env._getMeta(varialbe);
    } catch {
      return null;
    }
  }

  lookup(varialbe: IdentifierValue): RuntimeValue {
    const env = this._resolve(varialbe);
    return env._get(varialbe);
  }

  private _resolve(variable: IdentifierValue): Environment {
    if (this._contains(variable)) return this;

    if (this._outer) return this._outer._resolve(variable);

    throw new Error(`Error: ${variable} was not declared in this scope`);
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
