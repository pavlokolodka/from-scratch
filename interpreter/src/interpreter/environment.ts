import type { RuntimeValue } from './interpreter.interface';

export class Environment {
  private readonly _outer: Environment | undefined;

  private readonly _env: Map<string, RuntimeValue>;

  constructor(env?: Environment) {
    this._outer = env;
    this._env = new Map();
  }

  assign(variable: string, value: RuntimeValue): void {
    const env = this._resolve(variable);
    env._set(variable, value);
  }

  declare(variable: string, value: RuntimeValue): void {
    if (this._env.has(variable)) {
      throw new Error(`Error: redeclaration of ${variable}`);
    }

    this._set(variable, value);
  }

  lookup(varialbe: string): RuntimeValue {
    const env = this._resolve(varialbe);
    return env._get(varialbe);
  }

  private _resolve(variable: string): Environment {
    if (this._env.has(variable)) return this;

    if (this._outer) return this._outer._resolve(variable);

    throw new Error(`Error: ${variable} was not declared in this scope`);
  }

  private _get(variable: string): RuntimeValue {
    return this._env.get(variable)!;
  }

  private _set(variable: string, value: RuntimeValue): void {
    this._env.set(variable, value);
  }
}
