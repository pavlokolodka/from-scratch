import type {
  AssignStatement,
  BlockStatement,
  ConstStatement,
  ExpressionStatement,
  Identifier,
  InfixExpression,
  LetStatement,
  Node,
  NumberLiteral,
} from '../parser/ast';
import type { RuntimeValue } from './interpreter.interface';
import { NodeKind, NumberOperator } from '../parser/ast';
import { Environment } from './environment';
import { RuntimeType } from './interpreter.interface';
import { IdentifierValue, IdentifierValueInternal } from './values/identifier.value';
import { NumberValue } from './values/number.value';
import { VoidValue } from './values/void.value';

export class Interpreter {
  eval(ast: Node, env: Environment): RuntimeValue {
    switch (ast.kind) {
      case NodeKind.NUMBER_LITERAL:
        return new NumberValue((ast as NumberLiteral).value);
      case NodeKind.LET_STATEMENT:
      case NodeKind.CONST_STATEMENT:
        return this._evalDeclareStmt(ast as LetStatement, env);
      case NodeKind.ASSIGN_STATEMENT:
        return this._evalAssignStmt(ast as AssignStatement, env);
      case NodeKind.IDENTIFIER:
        return this._evalIdentifier(ast as Identifier, env);
      case NodeKind.INFIX_EXPRESSION:
        return this._evalInfix(ast as InfixExpression, env);
      case NodeKind.EXPRESSION_STATEMENT:
        return this.eval((ast as ExpressionStatement).expression, env);
      case NodeKind.BLOCK_STATEMENT:
        return this._evalBlockStmt(ast as BlockStatement, env);
      default:
        throw new Error(`AST have no implementation ${JSON.stringify(ast)}`);
    }
  }

  private _evalBlockStmt(stmt: BlockStatement, env: Environment): RuntimeValue {
    const localEnv = new Environment(env);
    const localStatements = stmt.statements;

    for (const lstmt of localStatements) {
      this.eval(lstmt, localEnv);
    }

    return VoidValue;
  }

  private _evalDeclareStmt(stmt: LetStatement | ConstStatement, env: Environment): RuntimeValue {
    const identifier = new IdentifierValue(stmt.left.value, stmt.kind);
    const value = this.eval(stmt.right, env);

    env.declare(identifier, value);

    return value;
  }

  private _evalAssignStmt(stmt: AssignStatement, env: Environment): RuntimeValue {
    const identifier = new IdentifierValueInternal(stmt.left.value);
    const value = this.eval(stmt.right, env);

    const meta = env.checkMeta(identifier);

    if (meta && meta.kind === NodeKind.CONST_STATEMENT) {
      throw new Error(`Error: assignment to constant variable`);
    }

    env.assign(identifier, value);

    return VoidValue;
  }

  private _evalIdentifier(node: Identifier, env: Environment): RuntimeValue {
    const identifier = new IdentifierValueInternal(node.value);
    return env.lookup(identifier);
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
