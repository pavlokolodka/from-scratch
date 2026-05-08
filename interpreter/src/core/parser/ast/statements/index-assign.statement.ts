import type { Token } from '../../../lexer/lexer.interface';
import type { Expression, Statement } from '../ast.interface';
import type { IndexExpression } from '../expressions';
import { NodeKind } from '../ast.interface';

export class IndexAssignStatement implements Statement {
  readonly kind = NodeKind.INDEX_ASSIGN_STATEMENT;

  constructor(
    public readonly token: Token,
    public readonly left: IndexExpression,
    public readonly right: Expression,
  ) {}

  get location() {
    return this.token.location;
  }

  get tokenLiteral(): string {
    return this.token.literal;
  }
}
