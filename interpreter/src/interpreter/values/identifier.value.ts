import type { NodeKind } from '../../parser/ast';
import type { RuntimeValue } from '../interpreter.interface';
import { RuntimeType } from '../interpreter.interface';

type VariableKind = NodeKind.LET_STATEMENT | NodeKind.CONST_STATEMENT | 'INTERNAL';

export class IdentifierValue implements RuntimeValue {
  readonly type = RuntimeType.IDENTIFIER;

  constructor(
    public readonly value: string,
    public readonly kind: VariableKind,
  ) {}
}

export class IdentifierValueInternal extends IdentifierValue {
  constructor(public readonly value: string) {
    super(value, 'INTERNAL');
  }
}

export class IdentifierValueMeta {
  constructor(public readonly kind: VariableKind) {}
}
