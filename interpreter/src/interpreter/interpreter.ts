import type {
  ArrayLiteral,
  AssignStatement,
  BlockStatement,
  CallExpression,
  ConstStatement,
  ExpressionStatement,
  FunctionDeclaration,
  Identifier,
  IndexExpression,
  InfixExpression,
  LetStatement,
  Node,
  NumberLiteral,
  ReturnStatement,
  StringLiteral,
} from '../parser/ast';
import type { RuntimeValue } from './interpreter.interface';
import { NodeKind, NumberOperator } from '../parser/ast';
import { Environment } from './environment';
import { RuntimeType } from './interpreter.interface';
import { ArrayValue } from './values/array.value';
import { FunctionValue } from './values/function.value';
import { IdentifierValue, IdentifierValueInternal } from './values/identifier.value';
import { NumberValue } from './values/number.value';
import { ReturnValue } from './values/return.value';
import { StringValue } from './values/string.value';
import { VoidValue } from './values/void.value';

export class Interpreter {
  private _callDepth = 0;
  eval(ast: Node, env: Environment): RuntimeValue {
    switch (ast.kind) {
      case NodeKind.NUMBER_LITERAL:
        return new NumberValue((ast as NumberLiteral).value);
      case NodeKind.STRING_LITERAL:
        return new StringValue((ast as StringLiteral).value);
      case NodeKind.ARRAY_LITERAL:
        return this._evalArrayLiteral(ast as ArrayLiteral, env);
      case NodeKind.INDEX_EXPRESSION:
        return this._evalIndexExpression(ast as IndexExpression, env);
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
      case NodeKind.FUNCTION_DECLARATION:
        return this._evalFunctionStmt(ast as FunctionDeclaration, env);
      case NodeKind.CALL_EXPRESSION:
        return this._evalCallExpr(ast as CallExpression, env);
      case NodeKind.RETURN_STATEMENT:
        return this._evalReturnStmt(ast as ReturnStatement, env);
      default:
        throw new Error(`AST have no implementation ${JSON.stringify(ast)}`);
    }
  }

  private _evalArrayLiteral(node: ArrayLiteral, env: Environment): ArrayValue {
    return new ArrayValue(node.elements.map((el) => this.eval(el, env)));
  }

  private _evalIndexExpression(node: IndexExpression, env: Environment): RuntimeValue {
    const array = this.eval(node.left, env);
    const index = this.eval(node.index, env);

    if (array.type !== RuntimeType.ARRAY) {
      throw new Error(`Index operator not supported for type ${array.type}`);
    }

    if (index.type !== RuntimeType.NUMBER) {
      throw new Error(`Index must be a number, got ${index.type}`);
    }

    const elements = (array as ArrayValue).value;
    const idx = (index as NumberValue).value;

    if (!Number.isInteger(idx) || idx < 0 || idx >= elements.length) {
      throw new Error(`Index out of bounds: ${idx}`);
    }

    return elements[idx];
  }

  private _evalReturnStmt(stmt: ReturnStatement, env: Environment): ReturnValue {
    if (this._callDepth === 0) {
      throw new Error('return outside of function');
    }
    return new ReturnValue(this.eval(stmt.value, env));
  }

  private _evalCallExpr(expr: CallExpression, env: Environment): RuntimeValue {
    const func = this._evalIdentifier(expr.identifier, env);

    if (!(func instanceof FunctionValue)) {
      throw new Error(`${expr.identifier.value} is not a function`);
    }

    if (expr.args.length !== func.parameters.length) {
      throw new Error(`Expected ${func.parameters.length} argument(s) but got ${expr.args.length}`);
    }

    const funcEnv = new Environment(func.environment);

    for (let i = 0; i < func.parameters.length; i++) {
      const param = new IdentifierValue(func.parameters[i].value, NodeKind.LET_STATEMENT);
      funcEnv.declare(param, this.eval(expr.args[i], env));
    }

    this._callDepth++;
    const result = this._evalBlockStmt(func.body, funcEnv);
    this._callDepth--;

    return result instanceof ReturnValue ? result.value : result;
  }

  private _evalFunctionStmt(stmt: FunctionDeclaration, env: Environment): RuntimeValue {
    const ident = new IdentifierValue(stmt.name.value, stmt.kind);
    const value = new FunctionValue(stmt.parameters, stmt.body, env);

    env.declare(ident, value);

    return VoidValue;
  }

  private _evalBlockStmt(stmt: BlockStatement, env: Environment): RuntimeValue {
    const localEnv = new Environment(env);

    for (const lstmt of stmt.statements) {
      const result = this.eval(lstmt, localEnv);
      if (result instanceof ReturnValue) return result;
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
