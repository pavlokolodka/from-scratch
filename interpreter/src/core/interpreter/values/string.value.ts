import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export class StringValue implements RuntimeValue {
  type = RuntimeType.STRING;

  constructor(public value: string) {}
}
