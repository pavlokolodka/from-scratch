import type {
  ArrayLiteral,
  AssignStatement,
  BlockStatement,
  BreakStatement,
  CallExpression,
  ConstStatement,
  FunctionDeclaration,
  Identifier,
  IfStatement,
  IndexAssignStatement,
  IndexExpression,
  InfixExpression,
  LetStatement,
  Node,
  PrefixExpression,
  Program,
  ReturnStatement,
  WhileStatement,
} from '../parser/ast';
import type { RuntimeValue } from './interpreter.interface';
import { isNode, NodeKind, NumberOperator, PrefixOperator } from '../parser/ast';
import { Environment } from './environment';
import { RuntimeType } from './interpreter.interface';
import { isType } from './is-type';
import { ArrayValue } from './values/array.value';
import { BooleanValue } from './values/boolean.value';
import { BreakValue } from './values/break.value';
import { FunctionValue } from './values/function.value';
import { IdentifierValue, IdentifierValueInternal } from './values/identifier.value';
import { NullValue } from './values/null.value';
import { NumberValue } from './values/number.value';
import { ReturnValue } from './values/return.value';
import { StringValue } from './values/string.value';
import { VoidValue } from './values/void.value';

export class Interpreter {
  private _callDepth = 0;

  private _loopDepth = 0;

  eval(ast: Node, env: Environment): RuntimeValue {
    if (isNode(ast, NodeKind.PROGRAM)) return this._evalProgram(ast, env);
    if (isNode(ast, NodeKind.NUMBER_LITERAL)) return new NumberValue(ast.value);
    if (isNode(ast, NodeKind.STRING_LITERAL)) return new StringValue(ast.value);
    if (isNode(ast, NodeKind.BOOLEAN_LITERAL)) return new BooleanValue(ast.value);
    if (isNode(ast, NodeKind.NULL_LITERAL)) return NullValue;
    if (isNode(ast, NodeKind.ARRAY_LITERAL)) return this._evalArrayLiteral(ast, env);
    if (isNode(ast, NodeKind.INDEX_EXPRESSION)) return this._evalIndexExpression(ast, env);
    if (isNode(ast, NodeKind.LET_STATEMENT) || isNode(ast, NodeKind.CONST_STATEMENT))
      return this._evalDeclareStmt(ast, env);
    if (isNode(ast, NodeKind.ASSIGN_STATEMENT)) return this._evalAssignStmt(ast, env);
    if (isNode(ast, NodeKind.INDEX_ASSIGN_STATEMENT)) return this._evalIndexAssignStmt(ast, env);
    if (isNode(ast, NodeKind.IDENTIFIER)) return this._evalIdentifier(ast, env);
    if (isNode(ast, NodeKind.PREFIX_EXPRESSION)) return this._evalPrefixExpression(ast, env);
    if (isNode(ast, NodeKind.INFIX_EXPRESSION)) return this._evalInfix(ast, env);
    if (isNode(ast, NodeKind.EXPRESSION_STATEMENT)) return this.eval(ast.expression, env);
    if (isNode(ast, NodeKind.BLOCK_STATEMENT)) return this._evalBlockStmt(ast, env);
    if (isNode(ast, NodeKind.FUNCTION_DECLARATION)) return this._evalFunctionStmt(ast, env);
    if (isNode(ast, NodeKind.IF_STATEMENT)) return this._evalIfStatement(ast, env);
    if (isNode(ast, NodeKind.WHILE_STATEMENT)) return this._evalWhileStatement(ast, env);
    if (isNode(ast, NodeKind.CALL_EXPRESSION)) return this._evalCallExpr(ast, env);
    if (isNode(ast, NodeKind.RETURN_STATEMENT)) return this._evalReturnStmt(ast, env);
    if (isNode(ast, NodeKind.BREAK_STATEMENT)) return this._evalBreakStmt(ast, env);

    throw new Error(`AST have no implementation ${JSON.stringify(ast)}`);
  }

  private _evalPrefixExpression(node: PrefixExpression, env: Environment): RuntimeValue {
    const right = this.eval(node.right, env);

    switch (node.operator) {
      case PrefixOperator.BANG:
        return new BooleanValue(!this._isTruthy(right));
      case PrefixOperator.DOUBLE_BANG:
        return new BooleanValue(this._isTruthy(right));
      case PrefixOperator.MINUS:
        if (!isType(right, RuntimeType.NUMBER)) {
          throw new Error(`Operator '-' not supported for type ${right.type}`);
        }
        return new NumberValue(-right.value);
      default:
        throw new Error(`Unknown operator: ${node.operator}`);
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

  private _evalBreakStmt(_stmt: BreakStatement, _env: Environment): BreakValue {
    if (this._loopDepth === 0) {
      throw new Error('stop outside of loop');
    }
    return new BreakValue(VoidValue);
  }

  private _evalReturnStmt(stmt: ReturnStatement, env: Environment): ReturnValue {
    if (this._callDepth === 0) {
      throw new Error('return outside of function');
    }
    return new ReturnValue(this.eval(stmt.value, env));
  }

  private _evalCallExpr(expr: CallExpression, env: Environment): RuntimeValue {
    const func = this._evalIdentifier(expr.identifier, env);

    if (isType(func, RuntimeType.BUILTIN_FN)) {
      const args = expr.args.map((arg) => this.eval(arg, env));
      return func.fn(args);
    }

    if (!isType(func, RuntimeType.FUNCTION)) {
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

    return isType(result, RuntimeType.RETURN) ? result.value : result;
  }

  private _evalFunctionStmt(stmt: FunctionDeclaration, env: Environment): RuntimeValue {
    const ident = new IdentifierValue(stmt.name.value, stmt.kind);
    const value = new FunctionValue(stmt.parameters, stmt.body, env);

    env.declare(ident, value);

    return VoidValue;
  }

  private _evalWhileStatement(node: WhileStatement, env: Environment): RuntimeValue {
    let condition = this.eval(node.condition, env);

    this._loopDepth++;

    while (this._isTruthy(condition)) {
      const result = this.eval(node.body, env);

      if (isType(result, RuntimeType.RETURN)) {
        this._loopDepth--;
        return result;
      }

      if (isType(result, RuntimeType.BREAK)) {
        break;
      }

      condition = this.eval(node.condition, env);
    }

    this._loopDepth--;

    return VoidValue;
  }

  private _evalIfStatement(node: IfStatement, env: Environment): RuntimeValue {
    const condition = this.eval(node.condition, env);

    if (this._isTruthy(condition)) {
      return this.eval(node.body, env);
    } else if (node.alternative) {
      return this.eval(node.alternative, env);
    }

    return VoidValue;
  }

  private _evalBlockStmt(stmt: BlockStatement, env: Environment): RuntimeValue {
    const localEnv = new Environment(env);

    for (const lstmt of stmt.statements) {
      const result = this.eval(lstmt, localEnv);
      if (isType(result, RuntimeType.RETURN) || isType(result, RuntimeType.BREAK)) return result;
    }

    return VoidValue;
  }

  private _evalDeclareStmt(stmt: LetStatement | ConstStatement, env: Environment): RuntimeValue {
    const identifier = new IdentifierValue(stmt.left.value, stmt.kind);
    const value = this.eval(stmt.right, env);

    env.declare(identifier, value);

    return value;
  }

  private _evalIndexAssignStmt(stmt: IndexAssignStatement, env: Environment): RuntimeValue {
    const array = this.eval(stmt.left.left, env);
    const index = this.eval(stmt.left.index, env);
    const value = this.eval(stmt.right, env);

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

    elements[idx] = value;

    return VoidValue;
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

    if (exp.operator === NumberOperator.EQ) {
      return new BooleanValue(left.value === right.value);
    }

    if (exp.operator === NumberOperator.NEQ) {
      return new BooleanValue(left.value !== right.value);
    }

    throw new Error(
      `Unexpected infix operands left: ${JSON.stringify(left)}, right: ${JSON.stringify(right)}`,
    );
  }

  private _evalNumber(left: number, right: number, operator: NumberOperator): RuntimeValue {
    switch (operator) {
      case NumberOperator.MINUS:
        return new NumberValue(left - right);
      case NumberOperator.PLUS:
        return new NumberValue(left + right);
      case NumberOperator.MULTIPLY:
        return new NumberValue(left * right);
      case NumberOperator.DIVIDE:
        return new NumberValue(left / right);
      case NumberOperator.LT:
        return new BooleanValue(left < right);
      case NumberOperator.GT:
        return new BooleanValue(left > right);
      case NumberOperator.LTE:
        return new BooleanValue(left <= right);
      case NumberOperator.GTE:
        return new BooleanValue(left >= right);
      case NumberOperator.EQ:
        return new BooleanValue(left === right);
      case NumberOperator.NEQ:
        return new BooleanValue(left !== right);
    }
  }

  private _isTruthy(val: RuntimeValue): boolean {
    if (isType(val, RuntimeType.NULL)) return false;
    if (isType(val, RuntimeType.BOOLEAN)) return val.value;
    if (isType(val, RuntimeType.STRING)) return val.value.length > 0;
    if (isType(val, RuntimeType.NUMBER)) return val.value !== 0;
    if (isType(val, RuntimeType.ARRAY)) return val.value.length > 0;
    return true;
  }

  private _evalProgram(program: Program, env: Environment): RuntimeValue {
    let lastEvaluated: RuntimeValue = VoidValue;

    for (const statement of program.statements) {
      const result = this.eval(statement, env);

      if (isType(result, RuntimeType.RETURN)) {
        return result.value;
      }

      if (isType(result, RuntimeType.BREAK)) {
        return result.value;
      }

      lastEvaluated = result;
    }

    return lastEvaluated;
  }
}
