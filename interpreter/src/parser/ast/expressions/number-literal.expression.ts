import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export enum NumberOperator {
  PLUS = '+',
  MINUS = '-',
  MULTIPLY = '*',
  DIVIDE = '/',
}

export class NumberLiteral implements Expression {
  kind = NodeKind.NUMBER_LITERAL;
  public value: number;

  constructor(public token: Token) {
    this.value = Number(token.literal);
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
}
