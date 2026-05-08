import type { NodeKind } from '../../parser/ast';
import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

const INTERNAL_VARIABLE = 'INTERNAL' as const;

type VariableKind =
  | NodeKind.LET_STATEMENT
  | NodeKind.CONST_STATEMENT
  | NodeKind.FUNCTION_DECLARATION
  | typeof INTERNAL_VARIABLE;

export class IdentifierValue implements RuntimeValue {
  readonly type = RuntimeType.IDENTIFIER;

  constructor(
    public readonly value: string,
    public readonly kind: VariableKind,
  ) {}
}

export class IdentifierValueInternal extends IdentifierValue {
  constructor(public readonly value: string) {
    super(value, INTERNAL_VARIABLE);
  }
}

export class IdentifierValueMeta {
  constructor(public readonly kind: VariableKind) {}
}
