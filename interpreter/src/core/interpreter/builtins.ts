import type { RuntimeValue } from './interpreter.interface';
import { RuntimeError } from '../errors';
import { RuntimeType } from './interpreter.interface';
import { isType, typeToString } from './runtime-type';
import { BuiltinFnValue } from './values/builtin-fn.value';
import { NumberValue } from './values/number.value';
import { StringValue } from './values/string.value';
import { VoidValue } from './values/void.value';

export interface BuiltInOptions {
  onPrint?: (...msg: string[]) => void;
}

export function stringifyOutput(val: RuntimeValue): string {
  if (isType(val, RuntimeType.NUMBER)) return val.value.toString();
  if (isType(val, RuntimeType.STRING)) return val.value;
  if (isType(val, RuntimeType.BOOLEAN)) return val.value.toString();
  if (isType(val, RuntimeType.NULL)) return 'nil';
  if (isType(val, RuntimeType.ARRAY)) {
    return `[${val.value.map(stringifyOutput).join(', ')}]`;
  }
  if (isType(val, RuntimeType.FUNCTION)) return '<function>';
  if (isType(val, RuntimeType.BUILTIN_FN)) return '<builtin_fn>';
  if (isType(val, RuntimeType.VOID)) return '<void>';
  return '';
}

export function createBuiltins(options: BuiltInOptions): Record<string, BuiltinFnValue> {
  const opt = toOptions(options);
  return {
    print: new BuiltinFnValue((args) => {
      opt.onPrint(...args.map(stringifyOutput));
      return VoidValue;
    }),
    len: new BuiltinFnValue((args, location) => {
      if (args.length !== 1) {
        throw new RuntimeError(`Expected 1 argument for len(), got ${args.length}`, location);
      }

      const arg = args[0];

      if (isType(arg, RuntimeType.STRING)) {
        return new NumberValue(arg.value.length);
      }

      if (isType(arg, RuntimeType.ARRAY)) {
        return new NumberValue(arg.value.length);
      }

      throw new RuntimeError(`len() not supported for type ${typeToString(arg.type)}`, location);
    }),
    str: new BuiltinFnValue((args, location) => {
      if (args.length !== 1) {
        throw new RuntimeError(`Expected 1 argument for str(), got ${args.length}`, location);
      }

      return new StringValue(stringifyOutput(args[0]));
    }),
    type: new BuiltinFnValue((args, location) => {
      if (args.length !== 1) {
        throw new RuntimeError(`Expected 1 argument for type(), got ${args.length}`, location);
      }

      return new StringValue(typeToString(args[0].type));
    }),
    push: new BuiltinFnValue((args, location) => {
      if (args.length !== 2) {
        throw new RuntimeError(`Expected 2 arguments for push(), got ${args.length}`, location);
      }

      const [target, val] = args;

      if (isType(target, RuntimeType.ARRAY)) {
        target.value.push(val);
        return new NumberValue(target.value.length);
      }

      if (isType(target, RuntimeType.STRING)) {
        target.value += stringifyOutput(val);
        return new NumberValue(target.value.length);
      }

      throw new RuntimeError(
        `push() not supported for type ${typeToString(target.type)}`,
        location,
      );
    }),
    num: new BuiltinFnValue((args, location) => {
      if (args.length !== 1) {
        throw new RuntimeError(`Expected 1 argument for num(), got ${args.length}`, location);
      }

      const arg = args[0];

      if (isType(arg, RuntimeType.NUMBER)) {
        return arg;
      }

      if (isType(arg, RuntimeType.STRING)) {
        const parsed = parseFloat(arg.value);
        if (Number.isNaN(parsed)) {
          throw new RuntimeError(`Could not convert string "${arg.value}" to number`, location);
        }
        return new NumberValue(parsed);
      }

      if (isType(arg, RuntimeType.BOOLEAN)) {
        return new NumberValue(arg.value ? 1 : 0);
      }

      throw new RuntimeError(`num() not supported for type ${typeToString(arg.type)}`, location);
    }),
  };
}

function toOptions(opt: BuiltInOptions): Required<BuiltInOptions> {
  return {
    onPrint: opt.onPrint ?? ((...args) => console.log(...args)),
  };
}
