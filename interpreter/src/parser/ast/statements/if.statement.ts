import type { Token } from '../../../lexer/lexer.interface';
import type { Expression, Statement } from '../ast.interface';
import type { BlockStatement } from './block.statement';
import { NodeKind } from '../ast.interface';

export class IfStatement implements Statement {
  readonly kind = NodeKind.IF_STATEMENT;

  constructor(
    public readonly token: Token,
    public readonly condition: Expression,
    public readonly consequence: BlockStatement,
    public readonly alternative: IfStatement | BlockStatement | null,
  ) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
