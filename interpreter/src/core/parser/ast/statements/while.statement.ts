import type { Token } from '../../../lexer/lexer.interface';
import type { Expression, Statement } from '../ast.interface';
import type { BlockStatement } from './block.statement';
import { NodeKind } from '../ast.interface';

export class WhileStatement implements Statement {
  readonly kind = NodeKind.WHILE_STATEMENT;

  constructor(
    public readonly token: Token,
    public readonly condition: Expression,
    public readonly body: BlockStatement,
  ) {}

  get location() {
    return this.token.location;
  }

  get tokenLiteral(): string {
    return this.token.literal;
  }
}
