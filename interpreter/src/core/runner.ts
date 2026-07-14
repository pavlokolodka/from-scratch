import type { BuiltInOptions } from './interpreter/builtins';
import type { RuntimeValue } from './interpreter/interpreter.interface';
import { Environment } from './interpreter/environment';
import { Interpreter } from './interpreter/interpreter';
import { Lexer } from './lexer/lexer';
import { Parser } from './parser/parser';

export interface RunnerOptions {
  builtIns?: BuiltInOptions;
}

export class Runner {
  private _env: Environment;
  private _interpreter: Interpreter;

  constructor(options: RunnerOptions = {}) {
    this._env = Environment.createGlobal(options.builtIns);
    this._interpreter = new Interpreter();
  }

  run(input: string): RuntimeValue {
    const program = this.parse(input);
    return this._interpreter.eval(program, this._env);
  }

  parse(input: string) {
    const lexer = new Lexer(input);
    const parser = new Parser(lexer.tokenize());
    return parser.parse();
  }
}
