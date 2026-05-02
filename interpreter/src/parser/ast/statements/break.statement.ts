import type { Token } from '../../../lexer/lexer.interface';
import type { Statement } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class BreakStatement implements Statement {
  readonly kind = NodeKind.BREAK_STATEMENT;

  constructor(public readonly token: Token) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
