import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class IndexExpression implements Expression {
  readonly kind = NodeKind.INDEX_EXPRESSION;

  constructor(
    public readonly token: Token,
    public readonly left: Expression,
    public readonly index: Expression,
  ) {}

  get location() {
    return this.token.location;
  }

  get tokenLiteral(): string {
    return this.token.literal;
  }
}
