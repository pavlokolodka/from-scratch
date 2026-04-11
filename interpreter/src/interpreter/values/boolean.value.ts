import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export class BooleanValue implements RuntimeValue {
  type = RuntimeType.BOOLEAN;

  constructor(public value: boolean) {}
}
