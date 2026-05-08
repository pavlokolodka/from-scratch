import type { BlockStatement, Identifier } from '../../parser/ast';
import type { Environment } from '../environment';
import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export class FunctionValue implements RuntimeValue {
  readonly type = RuntimeType.FUNCTION;
  readonly value = null;

  constructor(
    readonly parameters: Identifier[],
    readonly body: BlockStatement,
    readonly environment: Environment,
  ) {}
}
