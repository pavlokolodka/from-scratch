import type { ExpressionStatement, InfixExpression, Node, NumberLiteral } from '../parser/ast';
import type { RuntimeValue } from './interpreter.interface';
import { NodeKind, NumberOperator } from '../parser/ast';
import { RuntimeType } from './interpreter.interface';
import { NumberValue } from './values/number.value';

export class Interpreter {
  eval(ast: Node): RuntimeValue {
    switch (ast.kind) {
      case NodeKind.NUMBER_LITERAL:
        return { type: RuntimeType.NUMBER, value: (ast as NumberLiteral).value };
      case NodeKind.INFIX_EXPRESSION:
        return this._evalInfix(ast as InfixExpression);
      case NodeKind.EXPRESSION_STATEMENT:
        return this.eval((ast as ExpressionStatement).expression);
      default:
        throw new Error(`AST have no implementation ${JSON.stringify(ast)}`);
    }
  }

  private _evalInfix(exp: InfixExpression): RuntimeValue {
    const left = this.eval(exp.left);
    const right = this.eval(exp.right);

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
