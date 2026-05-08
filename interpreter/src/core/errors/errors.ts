import type { SourceLocation } from './error.interface';
import { ErrorType } from './error.interface';

export class InterpreterError extends Error {
  constructor(
    public readonly type: ErrorType,
    public readonly message: string,
    public readonly location: SourceLocation,
  ) {
    const { line, column } = location;
    super(`${type} [${line}:${column}]: ${message}`);
    this.name = 'InterpreterError';
  }
}

export class LexicalError extends InterpreterError {
  constructor(
    public readonly message: string,
    public readonly location: SourceLocation,
  ) {
    super(ErrorType.LEXICAL, message, location);
  }
}

export class ParseError extends InterpreterError {
  constructor(
    public readonly message: string,
    public readonly location: SourceLocation,
  ) {
    super(ErrorType.PARSE, message, location);
  }
}

export class RuntimeError extends InterpreterError {
  constructor(
    public readonly message: string,
    public readonly location: SourceLocation,
  ) {
    super(ErrorType.RUNTIME, message, location);
  }
}

export class ScopeError extends InterpreterError {
  constructor(
    public readonly message: string,
    public readonly location: SourceLocation,
  ) {
    super(ErrorType.SCOPE, message, location);
  }
}

export class ImmutableError extends InterpreterError {
  constructor(
    public readonly message: string,
    public readonly location: SourceLocation,
  ) {
    super(ErrorType.IMMUTABLE, message, location);
  }
}
