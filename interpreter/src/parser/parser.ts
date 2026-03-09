import type { Token } from '../lexer/lexer.interface';
import type { Expression, Statement } from './ast';
import { TokenType } from '../lexer/lexer.interface';
import {
  AssignStatement,
  ExpressionStatement,
  Identifier,
  InfixExpression,
  LetStatement,
  NumberLiteral,
  Program,
} from './ast';

export class Parser {
  private _tokens: Token[] = [];

  private _pos = 0;

  private _currentToken!: Token;

  private _peekToken!: Token;

  private _lowPrecedence = 0;

  constructor(tokens: Token[]) {
    this._tokens = tokens;
    this._nextToken();
    this._nextToken();
  }

  private _nextToken() {
    this._currentToken = this._peekToken;
    this._peekToken = this._tokens[this._pos] || {
      type: TokenType.EOF,
      literal: '',
      line: this._currentToken?.line || 1,
    };
    if (this._pos < this._tokens.length) {
      this._pos++;
    }
  }

  parse(): Program {
    const program = new Program();

    while (this._currentToken.type !== TokenType.EOF) {
      const stmt = this._parseStatement();
      if (stmt !== null) {
        program.statements.push(stmt);
      }
      this._nextToken();
    }

    return program;
  }

  private _parseStatement(): Statement | null {
    switch (this._currentToken.type) {
      case TokenType.LET:
        return this._parseLetStatement();
      case TokenType.IDENT:
        if (this._peekTokenIs(TokenType.ASSIGN)) {
          return this._parseAssignStatement();
        }
        return this._parseExpressionStatement();
      default:
        return this._parseExpressionStatement();
    }
  }

  private _parseLetStatement(): LetStatement | null {
    const token = this._currentToken;

    if (!this._expectPeek(TokenType.IDENT)) {
      return null;
    }

    const identifier = new Identifier(this._currentToken);

    if (!this._expectPeek(TokenType.ASSIGN)) {
      return null;
    }

    this._nextToken();

    const value = this._parseExpression(this._lowPrecedence);

    if (this._peekTokenIs(TokenType.SEMICOLON)) {
      this._nextToken();
    }

    return new LetStatement(token, identifier, value);
  }

  private _parseExpressionStatement(): ExpressionStatement {
    const token = this._currentToken;
    const expression = this._parseExpression(this._lowPrecedence);

    if (this._peekTokenIs(TokenType.SEMICOLON)) {
      this._nextToken();
    }

    return new ExpressionStatement(token, expression);
  }

  private _parseAssignStatement(): AssignStatement {
    const token = this._currentToken;
    const identifier = new Identifier(this._currentToken);

    this._nextToken();
    this._nextToken();

    const value = this._parseExpression(this._lowPrecedence);

    if (this._peekTokenIs(TokenType.SEMICOLON)) {
      this._nextToken();
    }

    return new AssignStatement(token, identifier, value);
  }

  private _parseExpression(precedence: number): Expression {
    const prefix = this._getPrefix();
    if (!prefix) {
      throw new Error(`No prefix parsing function for ${this._currentToken.type} found`);
    }

    let leftExp = prefix;

    while (
      !this._peekTokenIs(TokenType.SEMICOLON) &&
      precedence < this._getPrecedence(this._peekToken)
    ) {
      this._nextToken();

      const infix = this._getInfix(leftExp);
      if (!infix) {
        return leftExp;
      }

      leftExp = infix;
    }

    return leftExp;
  }

  private _getPrefix(): Expression | undefined {
    switch (this._currentToken.type) {
      case TokenType.IDENT:
        return new Identifier(this._currentToken);
      case TokenType.NUMBER:
        return new NumberLiteral(this._currentToken);
      default:
        return undefined;
    }
  }

  private _getInfix(left: Expression): Expression | undefined {
    switch (this._currentToken.type) {
      case TokenType.PLUS:
      case TokenType.MINUS:
      case TokenType.MULTIPLY:
      case TokenType.DIVIDE:
        return this._parseInfixExpression(left);
      default:
        return undefined;
    }
  }

  private _parseInfixExpression(left: Expression): Expression {
    const token = this._currentToken;
    const precedence = this._getPrecedence(token);

    this._nextToken();

    const right = this._parseExpression(precedence);

    return new InfixExpression(token, left, right);
  }

  private _getPrecedence(token: Token): number {
    switch (token.type) {
      case TokenType.MINUS:
      case TokenType.PLUS:
        return 1;
      case TokenType.MULTIPLY:
      case TokenType.DIVIDE:
        return 2;
      default:
        return 0;
    }
  }

  private _peekTokenIs(t: TokenType): boolean {
    return this._peekToken.type === t;
  }

  private _expectPeek(t: TokenType): boolean {
    if (this._peekTokenIs(t)) {
      this._nextToken();
      return true;
    } else {
      return false;
    }
  }
}
