import type { Token } from '../lexer/lexer.interface';

declare const __DEV__: boolean;

export { __DEV__ };

export function toDebugToken(token: Token): string {
  return `[${token.type} '${token.literal}' line:${token.line}]`;
}
