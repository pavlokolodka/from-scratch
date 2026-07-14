import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class BooleanLiteral implements Expression {
  readonly kind = NodeKind.BOOLEAN_LITERAL;
  readonly value: boolean;

  constructor(public readonly token: Token) {
    this.value = token.literal === 'true';
  }

  get location() {
    return this.token.location;
  }

  get tokenLiteral(): string {
    return this.token.literal;
  }
}
