import type { Token } from '../lexer/lexer.interface';

export function toDebugToken(token: Token): string {
  return `[type:${token.type} val:'${token.literal}' line:${token.line}]`;
}
