import type { Token } from '../lexer/lexer.interface';
import type { Node } from './ast';

export function toDebugToken(token: Token): string {
  return `[type:${token.type} val:'${token.literal}' line:${token.location.line}]`;
}

export function toDebugAST(node: Node): string {
  return JSON.stringify(
    node,
    (key, value) => {
      if (key === 'token' || key === 'location') {
        return undefined;
      }
      return value;
    },
    2,
  );
}
