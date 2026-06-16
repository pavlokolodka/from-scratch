import type { Token } from './lexer.interface';
import { LexicalError } from '../errors';
import { TokenType } from './lexer.interface';

export class Lexer {
  private _input: string;

  private _position = 0;

  private _readPosition = 0;

  private _char = '';

  private _line = 1;

  private _column = 0;

  private readonly _reservedKeywords: Record<string, TokenType> = {
    let: TokenType.LET,
    const: TokenType.CONST,
    if: TokenType.IF,
    elif: TokenType.ELIF,
    else: TokenType.ELSE,
    while: TokenType.WHILE,
    fn: TokenType.FUNCTION,
    nil: TokenType.NULL,
    true: TokenType.TRUE,
    false: TokenType.FALSE,
    return: TokenType.RETURN,
    stop: TokenType.BREAK,
  };

  constructor(input: string) {
    this._input = input;
    this._readChar();
  }

  tokenize(): Token[] {
    const tokens: Token[] = [];
    let token: Token;

    do {
      token = this._tokenize();
      tokens.push(token);
    } while (token.type !== TokenType.EOF);

    return tokens;
  }

  private _tokenize(): Token {
    this._skipToNextToken();

    const startLine = this._line;
    const startColumn = this._column;
    const startOffset = this._position;

    if (this._isLetter(this._char)) {
      const literal = this._readIdentifier();
      const type = this._lookupIdent(literal);
      return this._buildToken(type, literal, startLine, startColumn, startOffset);
    }

    if (this._isDigit(this._char)) {
      const literal = this._readNumber();
      return this._buildToken(TokenType.NUMBER, literal, startLine, startColumn, startOffset);
    }

    let token: Token;

    switch (this._char) {
      case '=':
        if (this._peekChar() === '=') {
          this._readChar();
          token = this._buildToken(TokenType.EQ, '==', startLine, startColumn, startOffset);
          break;
        }

        token = this._buildToken(TokenType.ASSIGN, this._char, startLine, startColumn, startOffset);
        break;
      case '!':
        if (this._peekChar() === '=') {
          this._readChar();
          token = this._buildToken(TokenType.NEQ, '!=', startLine, startColumn, startOffset);
          break;
        }

        if (this._peekChar() === '!') {
          this._readChar();
          token = this._buildToken(
            TokenType.DOUBLE_BANG,
            '!!',
            startLine,
            startColumn,
            startOffset,
          );
          break;
        }

        token = this._buildToken(TokenType.BANG, this._char, startLine, startColumn, startOffset);
        break;
      case '+':
        token = this._buildToken(TokenType.PLUS, this._char, startLine, startColumn, startOffset);
        break;
      case '-':
        token = this._buildToken(TokenType.MINUS, this._char, startLine, startColumn, startOffset);
        break;
      case '*':
        token = this._buildToken(
          TokenType.MULTIPLY,
          this._char,
          startLine,
          startColumn,
          startOffset,
        );
        break;
      case '/':
        token = this._buildToken(TokenType.DIVIDE, this._char, startLine, startColumn, startOffset);
        break;
      case '(':
        token = this._buildToken(TokenType.LPAREN, this._char, startLine, startColumn, startOffset);
        break;
      case ')':
        token = this._buildToken(TokenType.RPAREN, this._char, startLine, startColumn, startOffset);
        break;
      case '{':
        token = this._buildToken(TokenType.LBRACE, this._char, startLine, startColumn, startOffset);
        break;
      case '}':
        token = this._buildToken(TokenType.RBRACE, this._char, startLine, startColumn, startOffset);
        break;
      case '[':
        token = this._buildToken(
          TokenType.LBRACKET,
          this._char,
          startLine,
          startColumn,
          startOffset,
        );
        break;
      case ']':
        token = this._buildToken(
          TokenType.RBRACKET,
          this._char,
          startLine,
          startColumn,
          startOffset,
        );
        break;
      case ',':
        token = this._buildToken(TokenType.COMMA, this._char, startLine, startColumn, startOffset);
        break;
      case ':':
        token = this._buildToken(TokenType.COLON, this._char, startLine, startColumn, startOffset);
        break;
      case ';':
        token = this._buildToken(
          TokenType.SEMICOLON,
          this._char,
          startLine,
          startColumn,
          startOffset,
        );
        break;
      case '<':
        if (this._peekChar() === '=') {
          const char = this._char;
          this._readChar();
          token = this._buildToken(
            TokenType.LTE,
            char + this._char,
            startLine,
            startColumn,
            startOffset,
          );
        } else {
          token = this._buildToken(TokenType.LT, this._char, startLine, startColumn, startOffset);
        }
        break;
      case '>':
        if (this._peekChar() === '=') {
          const char = this._char;
          this._readChar();
          token = this._buildToken(
            TokenType.GTE,
            char + this._char,
            startLine,
            startColumn,
            startOffset,
          );
        } else {
          token = this._buildToken(TokenType.GT, this._char, startLine, startColumn, startOffset);
        }
        break;
      case '"':
        token = this._buildToken(
          TokenType.STRING,
          this._readString(),
          startLine,
          startColumn,
          startOffset,
        );
        break;
      case '':
        token = this._buildToken(TokenType.EOF, this._char, startLine, startColumn, startOffset);
        break;
      default:
        token = this._buildToken(
          TokenType.ILLEGAL,
          this._char,
          startLine,
          startColumn,
          startOffset,
        );
    }

    this._readChar();
    return token;
  }

  private _readChar() {
    if (this._char === '\n') {
      this._line += 1;
      this._column = 0;
    }

    if (this._readPosition >= this._input.length) {
      this._char = '';
    } else {
      this._char = this._input[this._readPosition];
    }
    this._position = this._readPosition;
    this._readPosition += 1;
    this._column += 1;
  }

  private _peekChar(): string {
    if (this._readPosition >= this._input.length) {
      return '';
    } else {
      return this._input[this._readPosition];
    }
  }

  private _readNumber(): string {
    const position = this._position;
    while (this._isDigit(this._char)) {
      this._readChar();
    }

    if (this._char === '.' && this._isDigit(this._peekChar())) {
      this._readChar();
      while (this._isDigit(this._char)) {
        this._readChar();
      }
    }

    return this._input.slice(position, this._position);
  }

  private _readString(): string {
    this._readChar();
    const position = this._position;
    while (this._char !== '"' && this._char !== '') {
      this._readChar();
    }

    if (this._char !== '"') {
      throw new LexicalError('Unclosed double quotes', {
        line: this._line,
        column: this._column,
        offset: this._position,
        length: 1,
      });
    }

    return this._input.slice(position, this._position);
  }

  private _readIdentifier(): string {
    const position = this._position;
    while (this._isLetter(this._char)) {
      this._readChar();
    }
    return this._input.slice(position, this._position);
  }

  private _skipToNextToken() {
    while (true) {
      if (this._char === ' ' || this._char === '\t' || this._char === '\n' || this._char === '\r') {
        this._readChar();
      } else if (this._char === '/' && this._peekChar() === '/') {
        this._nextLine();
      } else {
        break;
      }
    }
  }

  private _nextLine() {
    while (this._char !== '\n' && this._char !== '') {
      this._readChar();
    }

    if (this._char === '\n') {
      this._readChar();
    }
  }

  private _lookupIdent(ident: string): TokenType {
    return this._reservedKeywords[ident] || TokenType.IDENT;
  }

  private _isLetter(ch: string): boolean {
    return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch === '_';
  }

  private _isDigit(ch: string): boolean {
    return '0' <= ch && ch <= '9';
  }

  private _buildToken(
    type: TokenType,
    literal: string,
    line: number,
    column: number,
    offset: number,
  ): Token {
    return {
      type,
      literal,
      location: {
        line,
        column,
        offset,
        length: literal.length,
      },
    };
  }
}
