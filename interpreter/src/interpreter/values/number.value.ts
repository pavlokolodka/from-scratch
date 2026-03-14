import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export class NumberValue implements RuntimeValue {
  type = RuntimeType.NUMBER;

  constructor(public value: number) {}
}
