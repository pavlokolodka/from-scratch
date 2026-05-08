import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import type { Identifier } from './identifier.expression';
import { NodeKind } from '../ast.interface';

export class CallExpression implements Expression {
  readonly kind = NodeKind.CALL_EXPRESSION;

  constructor(
    public readonly token: Token,
    public readonly identifier: Identifier,
    public readonly args: Expression[],
  ) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
