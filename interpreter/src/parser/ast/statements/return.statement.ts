import type { Token } from '../../../lexer/lexer.interface';
import type { Expression, Statement } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class ReturnStatement implements Statement {
  readonly kind = NodeKind.RETURN_STATEMENT;

  constructor(
    public readonly token: Token,
    public readonly value: Expression,
  ) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
