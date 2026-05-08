import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class NullLiteral implements Expression {
  readonly kind = NodeKind.NULL_LITERAL;

  constructor(public readonly token: Token) {}

  get location() {
    return this.token.location;
  }

  get tokenLiteral(): string {
    return this.token.literal;
  }
}
