import type { Token } from '../../../lexer/lexer.interface';
import type { Expression, Statement } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class ExpressionStatement implements Statement {
  kind = NodeKind.EXPRESSION_STATEMENT;
  constructor(
    public token: Token,
    public expression: Expression,
  ) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
