import type { RuntimeType, RuntimeValue, RuntimeValueMap } from './interpreter.interface';

export function isType<T extends RuntimeType>(
  value: RuntimeValue,
  t: T,
): value is RuntimeValueMap[T] {
  return value.type === t;
}
