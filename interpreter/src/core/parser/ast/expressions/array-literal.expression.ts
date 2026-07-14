import type { Token } from '../../../lexer/lexer.interface';
import type { Expression } from '../ast.interface';
import { NodeKind } from '../ast.interface';

export class ArrayLiteral implements Expression {
  readonly kind = NodeKind.ARRAY_LITERAL;

  constructor(
    public readonly token: Token,
    public readonly elements: Expression[],
  ) {}

  get location() {
    return this.token.location;
  }

  get tokenLiteral(): string {
    return this.token.literal;
  }
}
