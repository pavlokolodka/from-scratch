import type { Token } from './lexer.interface';
import { TokenType } from './lexer.interface';

export class Lexer {
  private _input: string;

  private _position: number = 0;

  private _readPosition: number = 0;

  private _char: string = '';

  private _line: number = 1;

  private readonly _reservedKeywords: Record<string, TokenType> = {
    let: TokenType.LET,
    const: TokenType.CONST,
    if: TokenType.IF,
    elif: TokenType.ELIF,
    else: TokenType.ELSE,
    while: TokenType.WHILE,
    fn: TokenType.FN,
    nil: TokenType.NIL,
    true: TokenType.TRUE,
    false: TokenType.FALSE,
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
    this._skipWhitespace();

    if (this._isLetter(this._char)) {
      const literal = this._readIdentifier();
      const type = this._lookupIdent(literal);
      return this._buildToken(type, literal);
    }

    if (this._isDigit(this._char)) {
      return this._buildToken(TokenType.NUMBER, this._readNumber());
    }

    let token: Token;

    switch (this._char) {
      case '=':
        token = this._buildToken(TokenType.ASSIGN, this._char);
        break;
      case '+':
        token = this._buildToken(TokenType.PLUS, this._char);
        break;
      case '-':
        token = this._buildToken(TokenType.MINUS, this._char);
        break;
      case '*':
        token = this._buildToken(TokenType.MULTIPLY, this._char);
        break;
      case '/':
        token = this._buildToken(TokenType.DIVIDE, this._char);
        break;
      case '(':
        token = this._buildToken(TokenType.LPAREN, this._char);
        break;
      case ')':
        token = this._buildToken(TokenType.RPAREN, this._char);
        break;
      case '{':
        token = this._buildToken(TokenType.LBRACE, this._char);
        break;
      case '}':
        token = this._buildToken(TokenType.RBRACE, this._char);
        break;
      case '[':
        token = this._buildToken(TokenType.LBRACKET, this._char);
        break;
      case ']':
        token = this._buildToken(TokenType.RBRACKET, this._char);
        break;
      case ',':
        token = this._buildToken(TokenType.COMMA, this._char);
        break;
      case ':':
        token = this._buildToken(TokenType.COLON, this._char);
        break;
      case ';':
        token = this._buildToken(TokenType.SEMICOLON, this._char);
        break;
      case '<':
        token = this._buildToken(TokenType.LT, this._char);
        break;
      case '>':
        token = this._buildToken(TokenType.GT, this._char);
        break;
      case '"':
        token = this._buildToken(TokenType.STRING, this._readString());
        break;
      case '':
        token = this._buildToken(TokenType.EOF, this._char);
        break;
      default:
        token = this._buildToken(TokenType.ILLEGAL, this._char);
    }

    this._readChar();
    return token;
  }

  private _readChar() {
    if (this._readPosition >= this._input.length) {
      this._char = '';
    } else {
      this._char = this._input[this._readPosition];
    }
    this._position = this._readPosition;
    this._readPosition += 1;
  }

  private _readNumber(): string {
    const position = this._position;
    while (this._isDigit(this._char)) {
      this._readChar();
    }
    return this._input.slice(position, this._position);
  }

  private _readString(): string {
    this._readChar();
    const position = this._position;
    while (this._char !== '"' && this._char !== '') {
      this._readChar();
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

  private _skipWhitespace() {
    while (
      this._char === ' ' ||
      this._char === '\t' ||
      this._char === '\n' ||
      this._char === '\r'
    ) {
      if (this._char === '\n') {
        this._line++;
      }
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

  private _buildToken(type: TokenType, ch: string): Token {
    return { type, literal: ch, line: this._line };
  }
}
