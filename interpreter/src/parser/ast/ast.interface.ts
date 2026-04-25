import type { ArrayLiteral } from './expressions/array-literal.expression';
import type { BooleanLiteral } from './expressions/boolean-literal.expression';
import type { CallExpression } from './expressions/call.expression';
import type { Identifier } from './expressions/identifier.expression';
import type { IndexExpression } from './expressions/index-expression.expression';
import type { InfixExpression } from './expressions/infix.expression';
import type { NullLiteral } from './expressions/null-literal.expression';
import type { NumberLiteral } from './expressions/number-literal.expression';
import type { PrefixExpression } from './expressions/prefix.expression';
import type { StringLiteral } from './expressions/string-literal.expression';
import type { Program } from './program.node';
import type { AssignStatement } from './statements/assign.statement';
import type { BlockStatement } from './statements/block.statement';
import type { ConstStatement } from './statements/const.statement';
import type { ExpressionStatement } from './statements/expression.statement';
import type { FunctionDeclaration } from './statements/function-declaration.statement';
import type { IfStatement } from './statements/if.statement';
import type { IndexAssignStatement } from './statements/index-assign.statement';
import type { LetStatement } from './statements/let.statement';
import type { ReturnStatement } from './statements/return.statement';

export enum NodeKind {
  PROGRAM = 'PROGRAM',

  // Statements
  LET_STATEMENT = 'LET_STATEMENT',
  CONST_STATEMENT = 'CONST_STATEMENT',
  RETURN_STATEMENT = 'RETURN_STATEMENT',
  ASSIGN_STATEMENT = 'ASSIGN_STATEMENT',
  INDEX_ASSIGN_STATEMENT = 'INDEX_ASSIGN_STATEMENT',
  EXPRESSION_STATEMENT = 'EXPRESSION_STATEMENT',
  BLOCK_STATEMENT = 'BLOCK_STATEMENT',
  FUNCTION_DECLARATION = 'FUNCTION_DECLARATION',
  IF_STATEMENT = 'IF_STATEMENT',

  // Expressions
  IDENTIFIER = 'IDENTIFIER',
  NUMBER_LITERAL = 'NUMBER_LITERAL',
  BOOLEAN_LITERAL = 'BOOLEAN_LITERAL',
  STRING_LITERAL = 'STRING_LITERAL',
  CHARACTER_LITERAL = 'CHARACTER_LITERAL',
  NULL_LITERAL = 'NULL_LITERAL',
  ARRAY_LITERAL = 'ARRAY_LITERAL',

  PREFIX_EXPRESSION = 'PREFIX_EXPRESSION',
  INFIX_EXPRESSION = 'INFIX_EXPRESSION',
  IF_EXPRESSION = 'IF_EXPRESSION',
  WHILE_EXPRESSION = 'WHILE_EXPRESSION',
  CALL_EXPRESSION = 'CALL_EXPRESSION',
  INDEX_EXPRESSION = 'INDEX_EXPRESSION',
}

export interface Node {
  kind: NodeKind;
  tokenLiteral(): string;
}

export interface Statement extends Node {}

export interface Expression extends Node {}

export type NodeKindMap = {
  [NodeKind.PROGRAM]: Program;
  [NodeKind.IDENTIFIER]: Identifier;
  [NodeKind.NUMBER_LITERAL]: NumberLiteral;
  [NodeKind.STRING_LITERAL]: StringLiteral;
  [NodeKind.BOOLEAN_LITERAL]: BooleanLiteral;
  [NodeKind.NULL_LITERAL]: NullLiteral;
  [NodeKind.ARRAY_LITERAL]: ArrayLiteral;
  [NodeKind.PREFIX_EXPRESSION]: PrefixExpression;
  [NodeKind.INFIX_EXPRESSION]: InfixExpression;
  [NodeKind.INDEX_EXPRESSION]: IndexExpression;
  [NodeKind.CALL_EXPRESSION]: CallExpression;
  [NodeKind.LET_STATEMENT]: LetStatement;
  [NodeKind.CONST_STATEMENT]: ConstStatement;
  [NodeKind.ASSIGN_STATEMENT]: AssignStatement;
  [NodeKind.INDEX_ASSIGN_STATEMENT]: IndexAssignStatement;
  [NodeKind.EXPRESSION_STATEMENT]: ExpressionStatement;
  [NodeKind.BLOCK_STATEMENT]: BlockStatement;
  [NodeKind.FUNCTION_DECLARATION]: FunctionDeclaration;
  [NodeKind.RETURN_STATEMENT]: ReturnStatement;
  [NodeKind.IF_STATEMENT]: IfStatement;
};
