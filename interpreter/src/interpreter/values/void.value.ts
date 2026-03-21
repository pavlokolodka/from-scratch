import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

export const VoidValue = { type: RuntimeType.VOID, value: null } as RuntimeValue;
