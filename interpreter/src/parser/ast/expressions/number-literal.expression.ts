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
  readonly kind = NodeKind.NUMBER_LITERAL;
  readonly value: number;

  constructor(public readonly token: Token) {
    this.value = Number(token.literal);
  }

  tokenLiteral(): string {
    return this.token.literal;
  }
}
