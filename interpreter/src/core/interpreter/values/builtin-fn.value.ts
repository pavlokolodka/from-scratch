import type { SourceLocation } from '../../errors/error.interface';
import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export type BuiltinFunction = (args: RuntimeValue[], location: SourceLocation) => RuntimeValue;

export class BuiltinFnValue implements RuntimeValue {
  readonly type = RuntimeType.BUILTIN_FN;

  readonly value = null;

  constructor(public readonly fn: BuiltinFunction) {}
}
