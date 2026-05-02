import type { ArrayValue } from './values/array.value';
import type { BooleanValue } from './values/boolean.value';
import type { BreakValue } from './values/break.value';
import type { BuiltinFnValue } from './values/builtin-fn.value';
import type { FunctionValue } from './values/function.value';
import type { IdentifierValue } from './values/identifier.value';
import type { NullValueClass } from './values/null.value';
import type { NumberValue } from './values/number.value';
import type { ReturnValue } from './values/return.value';
import type { StringValue } from './values/string.value';
import type { VoidValueClass } from './values/void.value';

export enum RuntimeType {
  NUMBER = 'NUMBER',
  STRING = 'STRING',
  BOOLEAN = 'BOOLEAN',
  IDENTIFIER = 'IDENTIFIER',
  NULL = 'NULL',
  VOID = 'VOID',
  FUNCTION = 'FUNCTION',
  BUILTIN_FN = 'BUILTIN_FN',
  ARRAY = 'ARRAY',
  RETURN = 'RETURN',
  BREAK = 'BREAK',
}

export interface RuntimeValue {
  type: RuntimeType;
  value: any;
}

export type RuntimeValueMap = {
  [RuntimeType.NUMBER]: NumberValue;
  [RuntimeType.STRING]: StringValue;
  [RuntimeType.BOOLEAN]: BooleanValue;
  [RuntimeType.NULL]: NullValueClass;
  [RuntimeType.VOID]: VoidValueClass;
  [RuntimeType.ARRAY]: ArrayValue;
  [RuntimeType.FUNCTION]: FunctionValue;
  [RuntimeType.BUILTIN_FN]: BuiltinFnValue;
  [RuntimeType.RETURN]: ReturnValue;
  [RuntimeType.BREAK]: BreakValue;
  [RuntimeType.IDENTIFIER]: IdentifierValue;
};
