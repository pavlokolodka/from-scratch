import type { Token } from '../../../lexer/lexer.interface';
import type { Statement } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class BlockStatement implements Statement {
  readonly kind = NodeKind.BLOCK_STATEMENT;
  constructor(
    public readonly token: Token,
    public readonly statements: Statement[],
  ) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
