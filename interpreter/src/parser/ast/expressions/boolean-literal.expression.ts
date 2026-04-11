import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class BooleanLiteral implements Expression {
  readonly kind = NodeKind.BOOLEAN_LITERAL;
  readonly value: boolean;

  constructor(public readonly token: Token) {
    this.value = token.literal === 'true';
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
}
