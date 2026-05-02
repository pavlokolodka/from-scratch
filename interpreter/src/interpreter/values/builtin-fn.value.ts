import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export type BuiltinFunction = (args: RuntimeValue[]) => RuntimeValue;

export class BuiltinFnValue implements RuntimeValue {
  readonly type = RuntimeType.BUILTIN_FN;

  readonly value = null;

  constructor(public readonly fn: BuiltinFunction) {}
}
