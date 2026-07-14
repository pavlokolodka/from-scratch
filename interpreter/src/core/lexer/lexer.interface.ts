import type { SourceLocation } from '../errors/error.interface';

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
  NULL = 'nil',
  TRUE = 'true',
  FALSE = 'false',
  RETURN = 'return',
  BREAK = 'stop',

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
  BANG = '!',
  DOUBLE_BANG = '!!',

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
  location: SourceLocation;
}
