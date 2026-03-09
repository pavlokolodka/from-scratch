import type { Token } from '../../../lexer/lexer.interface';
import type { Expression, Statement } from '../ast.interface';
import type { Identifier } from '../expressions';
import { NodeKind } from '../ast.interface';

export class LetStatement implements Statement {
  kind = NodeKind.LET_STATEMENT;
  constructor(
    public token: Token,
    public left: Identifier,
    public right: Expression,
  ) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
