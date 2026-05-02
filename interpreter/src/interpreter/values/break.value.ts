import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export class BreakValue implements RuntimeValue {
  readonly type = RuntimeType.BREAK;

  constructor(public readonly value: RuntimeValue) {}
}
