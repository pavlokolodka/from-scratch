import type { Token } from '../../../lexer/lexer.interface.ts';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class InfixExpression implements Expression {
  readonly kind = NodeKind.INFIX_EXPRESSION;
  readonly operator: string;

  constructor(
    public readonly token: Token,
    public readonly left: Expression,
    public readonly right: Expression,
  ) {
    this.operator = token.literal;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
}
