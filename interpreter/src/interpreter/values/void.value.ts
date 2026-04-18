import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export class VoidValueClass implements RuntimeValue {
  readonly type = RuntimeType.VOID;
  readonly value = null;
}

export const VoidValue = new VoidValueClass();
