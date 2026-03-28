import type { Token } from '../../../lexer/lexer.interface';
import type { Statement } from '../ast.interface';
import type { Identifier } from '../expressions/identifier.expression';
import type { BlockStatement } from './block.statement';
import { NodeKind } from '../ast.interface';

export class FunctionDeclaration implements Statement {
  readonly kind = NodeKind.FUNCTION_DECLARATION;

  constructor(
    public readonly token: Token,
    public readonly name: Identifier,
    public readonly parameters: Identifier[],
    public readonly body: BlockStatement,
  ) {}

  tokenLiteral(): string {
    return this.token.literal;
  }
}
