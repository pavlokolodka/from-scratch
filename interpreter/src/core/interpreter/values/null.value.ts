import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export class NullValueClass implements RuntimeValue {
  readonly type = RuntimeType.NULL;
  readonly value = null;
}

export const NullValue = new NullValueClass();
