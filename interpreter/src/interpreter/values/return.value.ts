import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export class ReturnValue implements RuntimeValue {
  readonly type = RuntimeType.RETURN;

  constructor(public readonly value: RuntimeValue) {}
}
