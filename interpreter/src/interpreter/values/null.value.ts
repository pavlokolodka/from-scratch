import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export const NullValue = { type: RuntimeType.NULL, value: null } as RuntimeValue;
