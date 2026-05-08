import type { Token } from '../../../lexer/lexer.interface';
import type { Expression, Statement } from '../ast.interface';
import type { Identifier } from '../expressions';
import { NodeKind } from '../ast.interface';

export class ConstStatement implements Statement {
  readonly kind = NodeKind.CONST_STATEMENT;
  constructor(
    public readonly token: Token,
    public readonly left: Identifier,
    public readonly right: Expression,
  ) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
