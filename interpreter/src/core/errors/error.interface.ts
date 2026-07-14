export interface SourceLocation {
  line: number;
  column: number;
  offset: number;
  length: number;
}

export enum ErrorType {
  LEXICAL = 'Lexical Error',
  PARSE = 'Parse Error',
  RUNTIME = 'Runtime Error',
  SCOPE = 'Scope Error',
  IMMUTABLE = 'Immutable Error',
}
