import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class Identifier implements Expression {
  kind = NodeKind.IDENTIFIER;
  public value: string;

  constructor(public token: Token) {
    this.value = token.literal;
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
}
