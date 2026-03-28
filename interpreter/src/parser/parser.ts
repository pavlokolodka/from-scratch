import type { Token } from '../lexer/lexer.interface';
import type { Expression, Statement } from './ast';
import { TokenType } from '../lexer/lexer.interface';
import {
  AssignStatement,
  BlockStatement,
  CallExpression,
  ConstStatement,
  ExpressionStatement,
  FunctionDeclaration,
  Identifier,
  InfixExpression,
  LetStatement,
  NumberLiteral,
  Program,
  ReturnStatement,
} from './ast';
import assert from 'node:assert';

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
      case TokenType.CONST:
        return this._parseConstStatement();
      case TokenType.LBRACE:
        return this._parseBlockStatement();
      case TokenType.FUNCTION:
        return this._parseFunctionStatement();
      case TokenType.RETURN:
        return this._parseReturnStatement();
      case TokenType.IDENT:
        if (this._peekTokenIs(TokenType.ASSIGN)) {
          return this._parseAssignStatement();
        }
        return this._parseExpressionStatement();
      default:
        return this._parseExpressionStatement();
    }
  }

  private _parseReturnStatement(): ReturnStatement {
    const token = this._currentToken;

    this._nextToken();

    if (this._currTokenIs(TokenType.EOF) || this._currTokenIs(TokenType.SEMICOLON)) {
      throw new Error('Expected expression after return');
    }

    const value = this._parseExpression(this._lowPrecedence);

    if (this._peekTokenIs(TokenType.SEMICOLON)) {
      this._nextToken();
    }

    return new ReturnStatement(token, value);
  }

  private _parseFunctionStatement(): FunctionDeclaration | null {
    const token = this._currentToken;

    if (!this._expectPeek(TokenType.IDENT)) {
      throw new Error('Function declaration require a function name');
    }

    const ident = new Identifier(this._currentToken);

    if (!this._expectPeek(TokenType.LPAREN)) {
      throw new Error(`Unexpected token ${this._currentToken}`);
    }

    if (!this._expectPeek(TokenType.IDENT) && !this._expectPeek(TokenType.RPAREN)) {
      throw new Error(`Unexpected token ${this._currentToken}`);
    }

    const parameters: Identifier[] = [];

    while (!this._currTokenIs(TokenType.RPAREN)) {
      parameters.push(new Identifier(this._currentToken));

      if (this._expectPeek(TokenType.COMMA)) {
        if (!this._expectPeek(TokenType.IDENT)) {
          throw new Error(`Expected parameter name after ',', got ${this._currentToken}`);
        }
        continue;
      }

      if (!this._expectPeek(TokenType.IDENT) && !this._expectPeek(TokenType.RPAREN)) {
        throw new Error(`Unexpected token ${this._currentToken}`);
      }
    }

    if (!this._expectPeek(TokenType.LBRACE)) {
      throw new Error(`Unexpected token ${this._currentToken}`);
    }

    const body = this._parseBlockStatement();

    if (!body) {
      throw new Error(`Unexpected empty body`);
    }

    return new FunctionDeclaration(token, ident, parameters, body);
  }

  private _parseBlockStatement(): BlockStatement | null {
    const token = this._currentToken;
    const statements: Statement[] = [];

    while (!this._peekTokenIs(TokenType.RBRACE)) {
      this._nextToken();

      const stm = this._parseStatement();
      if (stm === null) {
        throw new Error('Closing parentheses not found');
      }

      statements.push(stm);
    }

    this._nextToken();

    return new BlockStatement(token, statements);
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

  private _parseConstStatement(): ConstStatement | null {
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

    return new ConstStatement(token, identifier, value);
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

  private _getPrefix(): Expression | null {
    switch (this._currentToken.type) {
      case TokenType.IDENT:
        return new Identifier(this._currentToken);
      case TokenType.NUMBER:
        return new NumberLiteral(this._currentToken);
      case TokenType.LPAREN:
        return this._parseLParen();
      default:
        return null;
    }
  }

  private _parseLParen(): Expression {
    this._nextToken();

    const expr = this._parseExpression(this._lowPrecedence);

    if (!this._peekTokenIs(TokenType.RPAREN)) {
      throw new Error('Closing parentheses not found');
    }

    this._nextToken();

    return expr;
  }

  private _getInfix(left: Expression): Expression | null {
    switch (this._currentToken.type) {
      case TokenType.PLUS:
      case TokenType.MINUS:
      case TokenType.MULTIPLY:
      case TokenType.DIVIDE:
        return this._parseInfixExpression(left);
      case TokenType.LPAREN:
        return this._parseCallExpression(left);
      default:
        return null;
    }
  }

  private _parseCallExpression(left: Expression): CallExpression {
    assert.ok(left instanceof Identifier, `Expected function name, got ${left.kind}`);

    const token = this._currentToken;
    const args: Expression[] = [];

    if (this._expectPeek(TokenType.RPAREN)) {
      return new CallExpression(token, left, args);
    }

    this._nextToken();

    if (this._currTokenIs(TokenType.COMMA)) {
      throw new Error(`Expected argument expression, got ','`);
    }
    if (this._currTokenIs(TokenType.EOF)) {
      throw new Error("Unclosed argument list: expected ')'");
    }

    args.push(this._parseExpression(this._lowPrecedence));

    while (this._peekTokenIs(TokenType.COMMA)) {
      this._nextToken();
      this._nextToken();

      if (this._currTokenIs(TokenType.RPAREN)) {
        throw new Error(`Expected argument expression after ',', got ')'`);
      }
      if (this._currTokenIs(TokenType.EOF)) {
        throw new Error("Unclosed argument list: expected ')'");
      }
      if (this._currTokenIs(TokenType.COMMA)) {
        throw new Error(`Expected argument expression, got ','`);
      }

      args.push(this._parseExpression(this._lowPrecedence));
    }

    if (!this._expectPeek(TokenType.RPAREN)) {
      throw new Error("Unclosed argument list: expected ')'");
    }

    return new CallExpression(token, left, args);
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
      case TokenType.LPAREN:
        return 3;
      case TokenType.MINUS:
      case TokenType.PLUS:
        return 1;
      case TokenType.MULTIPLY:
      case TokenType.DIVIDE:
        return 2;
      default:
        return this._lowPrecedence;
    }
  }

  private _currTokenIs(t: TokenType): boolean {
    return this._currentToken.type === t;
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
