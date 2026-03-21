import type {
  AssignStatement,
  ExpressionStatement,
  Identifier,
  InfixExpression,
  LetStatement,
  Node,
  NumberLiteral,
} from '../parser/ast';
import type { Environment } from './environment';
import type { RuntimeValue } from './interpreter.interface';
import { NodeKind, NumberOperator } from '../parser/ast';
import { RuntimeType } from './interpreter.interface';
import { NumberValue } from './values/number.value';
import { VoidValue } from './values/void.value';

export class Interpreter {
  eval(ast: Node, env: Environment): RuntimeValue {
    switch (ast.kind) {
      case NodeKind.NUMBER_LITERAL:
        return new NumberValue((ast as NumberLiteral).value);
      case NodeKind.LET_STATEMENT:
        return this._evalLetStmt(ast as LetStatement, env);
      case NodeKind.ASSIGN_STATEMENT:
        return this._evalAssignStmt(ast as AssignStatement, env);
      case NodeKind.IDENTIFIER:
        return this._evalIdentifier(ast as Identifier, env);
      case NodeKind.INFIX_EXPRESSION:
        return this._evalInfix(ast as InfixExpression, env);
      case NodeKind.EXPRESSION_STATEMENT:
        return this.eval((ast as ExpressionStatement).expression, env);
      default:
        throw new Error(`AST have no implementation ${JSON.stringify(ast)}`);
    }
  }

  private _evalLetStmt(stmt: LetStatement, env: Environment): RuntimeValue {
    const identifier = stmt.left.value;
    const value = this.eval(stmt.right, env);

    env.declare(identifier, value);

    return value;
  }

  private _evalAssignStmt(stmt: AssignStatement, env: Environment): RuntimeValue {
    const identifier = stmt.left.value;
    const value = this.eval(stmt.right, env);

    env.assign(identifier, value);

    return VoidValue;
  }

  private _evalIdentifier(node: Identifier, env: Environment): RuntimeValue {
    return env.lookup(node.value);
  }

  private _evalInfix(exp: InfixExpression, env: Environment): RuntimeValue {
    const left = this.eval(exp.left, env);
    const right = this.eval(exp.right, env);

    if (left.type === RuntimeType.NUMBER && right.type === RuntimeType.NUMBER) {
      return this._evalNumber(left.value, right.value, exp.operator as NumberOperator);
    }

    throw new Error(
      `Unexpected infix operands left: ${JSON.stringify(left)}, right: ${JSON.stringify(right)}`,
    );
  }

  private _evalNumber(left: number, right: number, operator: NumberOperator): NumberValue {
    switch (operator) {
      case NumberOperator.MINUS:
        return new NumberValue(left - right);
      case NumberOperator.PLUS:
        return new NumberValue(left + right);
      case NumberOperator.MULTIPLY:
        return new NumberValue(left * right);
      case NumberOperator.DIVIDE:
        return new NumberValue(left / right);
    }
  }
}
