import type { Token } from '../../../lexer/lexer.interface.ts';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class InfixExpression implements Expression {
  kind = NodeKind.INFIX_EXPRESSION;
  public operator: string;

  constructor(
    public token: Token,
    public left: Expression,
    public right: Expression,
  ) {
    this.operator = token.literal;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
}
