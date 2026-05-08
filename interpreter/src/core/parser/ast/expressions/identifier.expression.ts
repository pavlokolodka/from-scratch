import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class Identifier implements Expression {
  readonly kind = NodeKind.IDENTIFIER;
  readonly value: string;

  constructor(public readonly token: Token) {
    this.value = token.literal;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
}
