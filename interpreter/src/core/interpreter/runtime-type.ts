import type { RuntimeValue, RuntimeValueMap } from './interpreter.interface';
import { RuntimeType } from './interpreter.interface';

export function isType<T extends RuntimeType>(
  value: RuntimeValue,
  t: T,
): value is RuntimeValueMap[T] {
  return value.type === t;
}

export function typeToString(val: RuntimeType): string {
  switch (val) {
    case RuntimeType.NUMBER:
      return 'number';
    case RuntimeType.STRING:
      return 'string';
    case RuntimeType.BOOLEAN:
      return 'boolean';
    case RuntimeType.NULL:
      return 'nil';
    case RuntimeType.VOID:
      return 'void';
    case RuntimeType.FUNCTION:
      return 'function';
    case RuntimeType.BUILTIN_FN:
      return 'builtin function';
    case RuntimeType.ARRAY:
      return 'array';
    case RuntimeType.IDENTIFIER:
      return 'identifier';
    case RuntimeType.RETURN:
      return 'return';
    case RuntimeType.BREAK:
      return 'break';
  }
}
