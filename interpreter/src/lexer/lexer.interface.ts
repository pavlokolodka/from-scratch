export enum TokenType {
  // Identifiers + literals
  IDENT = 'IDENT',
  NUMBER = 'NUMBER',
  STRING = 'STRING',
  CHARACTER = 'CHARACTER',

  // Keywords
  LET = 'let',
  CONST = 'const',
  IF = 'if',
  ELIF = 'elif',
  ELSE = 'else',
  WHILE = 'while',
  FUNCTION = 'fn',
  NIL = 'nil',
  TRUE = 'true',
  FALSE = 'false',
  RETURN = 'return',

  // Operators
  ASSIGN = '=',
  PLUS = '+',
  MINUS = '-',
  MULTIPLY = '*',
  DIVIDE = '/',
  LT = '<',
  GT = '>',
  EQ = '==',
  NEQ = '!=',
  LTE = '<=',
  GTE = '>=',

  // Punctuation
  COMMA = ',',
  COLON = ':',
  SEMICOLON = ';',
  LPAREN = '(',
  RPAREN = ')',
  LBRACE = '{',
  RBRACE = '}',
  LBRACKET = '[',
  RBRACKET = ']',

  // Special
  EOF = 'EOF',
  ILLEGAL = 'ILLEGAL',
}

export interface Token {
  type: TokenType;
  literal: string;
  line: number;
}
