import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class Identifier implements Expression {
  readonly kind = NodeKind.IDENTIFIER;
  readonly value: string;

  constructor(public readonly token: Token) {
    this.value = token.literal;
  }

  get location() {
    return this.token.location;
  }

  get tokenLiteral(): string {
    return this.token.literal;
  }
}
