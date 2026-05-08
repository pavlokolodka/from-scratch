import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export class ArrayValue implements RuntimeValue {
  type = RuntimeType.ARRAY;

  constructor(public value: RuntimeValue[]) {}
}
