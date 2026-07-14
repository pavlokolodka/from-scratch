import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export enum PrefixOperator {
  MINUS = '-',
  BANG = '!',
  DOUBLE_BANG = '!!',
}

export class PrefixExpression implements Expression {
  readonly kind = NodeKind.PREFIX_EXPRESSION;
  readonly operator: PrefixOperator;

  constructor(
    public readonly token: Token,
    public readonly right: Expression,
  ) {
    this.operator = token.literal as PrefixOperator;
  }

  get location() {
    return this.token.location;
  }

  get tokenLiteral(): string {
    return this.token.literal;
  }
}
