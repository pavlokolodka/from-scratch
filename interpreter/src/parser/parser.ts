import type { Token } from '../lexer/lexer.interface';
import type { Expression, Statement } from './ast';
import { TokenType } from '../lexer/lexer.interface';
import {
  ArrayLiteral,
  AssignStatement,
  BlockStatement,
  BooleanLiteral,
  CallExpression,
  ConstStatement,
  ExpressionStatement,
  FunctionDeclaration,
  Identifier,
  IndexAssignStatement,
  IndexExpression,
  InfixExpression,
  isNode,
  LetStatement,
  NodeKind,
  NullLiteral,
  NumberLiteral,
  Program,
  ReturnStatement,
  StringLiteral,
} from './ast';
import { __DEV__, toDebugToken } from './debug';
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
      case TokenType.SEMICOLON:
        return null;
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

  private _parseArray(): ArrayLiteral {
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.LBRACKET),
        `_parseArrayStatement called with non-Array token: ${toDebugToken(this._currentToken)}`,
      );

    const token = this._currentToken;

    if (!this._expectPeekArrayToken() && !this._expectPeek(TokenType.RBRACKET)) {
      throw new Error(`Unexpected token ${toDebugToken(this._currentToken)}`);
    }

    const expression: Expression[] = [];

    while (!this._currTokenIs(TokenType.RBRACKET)) {
      expression.push(this._parseExpression(this._lowPrecedence));

      if (this._expectPeek(TokenType.COMMA)) {
        if (!this._expectPeekArrayToken()) {
          throw new Error(
            `Expected array element after ',', got ${toDebugToken(this._currentToken)}`,
          );
        }
        continue;
      }

      if (!this._expectPeekArrayToken() && !this._expectPeek(TokenType.RBRACKET)) {
        throw new Error(`Unexpected token ${toDebugToken(this._currentToken)}`);
      }
    }

    return new ArrayLiteral(token, expression);
  }

  private _parseReturnStatement(): ReturnStatement {
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.RETURN),
        `_parseReturnStatement called with non-RETURN token: ${toDebugToken(this._currentToken)}`,
      );

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

  private _parseFunctionStatement(): FunctionDeclaration {
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.FUNCTION),
        `_parseFunctionStatement called with non-FUNCTION token: ${toDebugToken(this._currentToken)}`,
      );

    const token = this._currentToken;

    if (!this._expectPeek(TokenType.IDENT)) {
      throw new Error('Function declaration require a function name');
    }

    const ident = new Identifier(this._currentToken);

    if (!this._expectPeek(TokenType.LPAREN)) {
      throw new Error(`Unexpected token ${toDebugToken(this._currentToken)}`);
    }

    if (!this._expectPeek(TokenType.IDENT) && !this._expectPeek(TokenType.RPAREN)) {
      throw new Error(`Unexpected token ${toDebugToken(this._currentToken)}`);
    }

    const parameters: Identifier[] = [];

    while (!this._currTokenIs(TokenType.RPAREN)) {
      parameters.push(new Identifier(this._currentToken));

      if (this._expectPeek(TokenType.COMMA)) {
        if (!this._expectPeek(TokenType.IDENT)) {
          throw new Error(
            `Expected parameter name after ',', got ${toDebugToken(this._currentToken)}`,
          );
        }
        continue;
      }

      if (!this._expectPeek(TokenType.IDENT) && !this._expectPeek(TokenType.RPAREN)) {
        throw new Error(`Unexpected token ${toDebugToken(this._currentToken)}`);
      }
    }

    if (!this._expectPeek(TokenType.LBRACE)) {
      throw new Error(`Unexpected token ${toDebugToken(this._currentToken)}`);
    }

    const body = this._parseBlockStatement();

    if (!body) {
      throw new Error(`Unexpected empty body`);
    }

    return new FunctionDeclaration(token, ident, parameters, body);
  }

  private _parseBlockStatement(): BlockStatement {
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.LBRACE),
        `_parseBlockStatement called with non-LBRACE token: ${toDebugToken(this._currentToken)}`,
      );

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

  private _parseLetStatement(): LetStatement {
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.LET),
        `_parseLetStatement called with non-LET token: ${toDebugToken(this._currentToken)}`,
      );

    const token = this._currentToken;

    if (!this._expectPeek(TokenType.IDENT)) {
      throw new Error(`Expected identifier in let statement, got ${toDebugToken(this._peekToken)}`);
    }

    const identifier = new Identifier(this._currentToken);

    if (!this._expectPeek(TokenType.ASSIGN)) {
      throw new Error(
        `Expected '=' after identifier in let statement, got ${toDebugToken(this._peekToken)}`,
      );
    }

    this._nextToken();

    const value = this._parseExpression(this._lowPrecedence);

    if (this._peekTokenIs(TokenType.SEMICOLON)) {
      this._nextToken();
    }

    return new LetStatement(token, identifier, value);
  }

  private _parseConstStatement(): ConstStatement {
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.CONST),
        `_parseConstStatement called with non-CONST token: ${toDebugToken(this._currentToken)}`,
      );

    const token = this._currentToken;

    if (!this._expectPeek(TokenType.IDENT)) {
      throw new Error(
        `Expected identifier in const statement, got ${toDebugToken(this._peekToken)}`,
      );
    }

    const identifier = new Identifier(this._currentToken);

    if (!this._expectPeek(TokenType.ASSIGN)) {
      throw new Error(
        `Expected '=' after identifier in const statement, got ${toDebugToken(this._peekToken)}`,
      );
    }

    this._nextToken();

    const value = this._parseExpression(this._lowPrecedence);

    if (this._peekTokenIs(TokenType.SEMICOLON)) {
      this._nextToken();
    }

    return new ConstStatement(token, identifier, value);
  }

  private _parseExpressionStatement(): Statement {
    const token = this._currentToken;
    const expression = this._parseExpression(this._lowPrecedence);

    if (isNode(expression, NodeKind.INDEX_EXPRESSION) && this._peekTokenIs(TokenType.ASSIGN)) {
      return this._parseIndexAssignStatement(token, expression);
    }

    if (this._peekTokenIs(TokenType.SEMICOLON)) {
      this._nextToken();
    }

    return new ExpressionStatement(token, expression);
  }

  private _parseIndexAssignStatement(t: Token, expression: IndexExpression): IndexAssignStatement {
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.RBRACKET),
        `_parseIndexAssignStatement called with non-RBRACKET token: ${toDebugToken(this._currentToken)}`,
      );

    this._nextToken();
    this._nextToken();

    const value = this._parseExpression(this._lowPrecedence);

    if (this._peekTokenIs(TokenType.SEMICOLON)) {
      this._nextToken();
    }

    return new IndexAssignStatement(t, expression, value);
  }

  private _parseAssignStatement(): AssignStatement {
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.IDENT),
        `_parseAssignStatement called with non-IDENT token: ${toDebugToken(this._currentToken)}`,
      );
    __DEV__ &&
      assert.ok(
        this._peekTokenIs(TokenType.ASSIGN),
        `_parseAssignStatement called without ASSIGN peek token: ${toDebugToken(this._peekToken)}`,
      );

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
      throw new Error(`No prefix parsing function for ${toDebugToken(this._currentToken)} found`);
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
      case TokenType.STRING:
        return new StringLiteral(this._currentToken);
      case TokenType.LPAREN:
        return this._parseLParen();
      case TokenType.LBRACKET:
        return this._parseArray();
      case TokenType.TRUE:
      case TokenType.FALSE:
        return new BooleanLiteral(this._currentToken);
      case TokenType.NIL:
        return new NullLiteral(this._currentToken);
      default:
        return null;
    }
  }

  private _parseLParen(): Expression {
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.LPAREN),
        `_parseLParen called with non-LPAREN token: ${toDebugToken(this._currentToken)}`,
      );

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
      case TokenType.LT:
      case TokenType.GT:
      case TokenType.LTE:
      case TokenType.GTE:
      case TokenType.EQ:
      case TokenType.NEQ:
        return this._parseInfixExpression(left);
      case TokenType.LPAREN:
        return this._parseCallExpression(left);
      case TokenType.LBRACKET:
        return this._parseIndexExpression(left);
      default:
        return null;
    }
  }

  private _parseIndexExpression(left: Expression): IndexExpression {
    const token = this._currentToken;

    this._nextToken();

    const index = this._parseExpression(this._lowPrecedence);

    if (!this._expectPeek(TokenType.RBRACKET)) {
      throw new Error('Missing closing bracket in index expression');
    }

    return new IndexExpression(token, left, index);
  }

  private _parseCallExpression(left: Expression): CallExpression {
    __DEV__ &&
      assert.ok(isNode(left, NodeKind.IDENTIFIER), `Expected function name, got ${left.kind}`);
    __DEV__ &&
      assert.ok(
        this._currTokenIs(TokenType.LPAREN),
        `_parseCallExpression called with non-LPAREN token: ${toDebugToken(this._currentToken)}`,
      );

    const token = this._currentToken;
    const ident = left as Identifier;
    const args: Expression[] = [];

    if (this._expectPeek(TokenType.RPAREN)) {
      return new CallExpression(token, ident, args);
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

    return new CallExpression(token, ident, args);
  }

  private _parseInfixExpression(left: Expression): Expression {
    __DEV__ &&
      assert.ok(
        [
          TokenType.PLUS,
          TokenType.MINUS,
          TokenType.MULTIPLY,
          TokenType.DIVIDE,
          TokenType.LT,
          TokenType.GT,
          TokenType.LTE,
          TokenType.GTE,
          TokenType.EQ,
          TokenType.NEQ,
        ].includes(this._currentToken.type as TokenType),
        `_parseInfixExpression called with non-operator token: ${toDebugToken(this._currentToken)}`,
      );

    const token = this._currentToken;
    const precedence = this._getPrecedence(token);

    this._nextToken();

    const right = this._parseExpression(precedence);

    return new InfixExpression(token, left, right);
  }

  private _getPrecedence(token: Token): number {
    switch (token.type) {
      case TokenType.LBRACKET:
      case TokenType.LPAREN:
        return 5;
      case TokenType.MULTIPLY:
      case TokenType.DIVIDE:
        return 4;
      case TokenType.MINUS:
      case TokenType.PLUS:
        return 3;
      case TokenType.LT:
      case TokenType.GT:
      case TokenType.LTE:
      case TokenType.GTE:
        return 2;
      case TokenType.EQ:
      case TokenType.NEQ:
        return 1;
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

  private _expectPeekArrayToken(): boolean {
    return (
      this._expectPeek(TokenType.IDENT) ||
      this._expectPeek(TokenType.STRING) ||
      this._expectPeek(TokenType.NUMBER) ||
      this._expectPeek(TokenType.TRUE) ||
      this._expectPeek(TokenType.FALSE) ||
      this._expectPeek(TokenType.NIL) ||
      this._expectPeek(TokenType.LBRACKET)
    );
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
