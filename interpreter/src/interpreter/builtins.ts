import type { RuntimeValue } from './interpreter.interface';
import { RuntimeType } from './interpreter.interface';
import { isType } from './is-type';
import { BuiltinFnValue } from './values/builtin.value';
import { VoidValue } from './values/void.value';

function stringify(val: RuntimeValue): string {
  if (isType(val, RuntimeType.NUMBER)) return val.value.toString();
  if (isType(val, RuntimeType.STRING)) return val.value;
  if (isType(val, RuntimeType.BOOLEAN)) return val.value.toString();
  if (isType(val, RuntimeType.NULL)) return 'nil';
  if (isType(val, RuntimeType.ARRAY)) {
    return `[${val.value.map(stringify).join(', ')}]`;
  }
  if (isType(val, RuntimeType.FUNCTION)) return '<function>';
  if (isType(val, RuntimeType.BUILTIN_FN)) return '<builtin_fn>';
  if (isType(val, RuntimeType.VOID)) return '<void>';
  return '';
}

export const builtins: Record<string, BuiltinFnValue> = {
  print: new BuiltinFnValue((args) => {
    console.log(...args.map(stringify));
    return VoidValue;
  }),
};
