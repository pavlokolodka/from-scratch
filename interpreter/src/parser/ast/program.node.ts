import type { Node, Statement } from './ast.interface';
import { NodeKind } from './ast.interface';

export class Program implements Node {
  readonly kind = NodeKind.PROGRAM;
  readonly statements: Statement[] = [];

  tokenLiteral(): string {
    if (this.statements.length > 0) {
      return this.statements[0].tokenLiteral();
    }
    return '';
  }
}
