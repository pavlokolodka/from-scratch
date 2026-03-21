import type { Token } from '../../../lexer/lexer.interface';
import type { Expression, Statement } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class ExpressionStatement implements Statement {
  readonly kind = NodeKind.EXPRESSION_STATEMENT;
  constructor(
    public readonly token: Token,
    public readonly expression: Expression,
  ) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
