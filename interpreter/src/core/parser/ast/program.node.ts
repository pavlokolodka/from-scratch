import type { SourceLocation } from '../../errors';
import type { Node, Statement } from './ast.interface';
import { NodeKind } from './ast.interface';

export class Program implements Node {
  readonly kind = NodeKind.PROGRAM;
  readonly statements: Statement[] = [];

  get location(): SourceLocation {
    if (this.statements.length > 0) {
      return this.statements[0].location;
    }
    return {
      line: 1,
      column: 1,
      offset: 0,
      length: 0,
    };
  }

  get tokenLiteral(): string {
    if (this.statements.length > 0) {
      return this.statements[0].tokenLiteral;
    }
    return '';
  }
}
