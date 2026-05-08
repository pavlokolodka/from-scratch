import type { InterpreterError } from './errors';

export class ErrorReporter {
  private constructor() {}

  static report(error: InterpreterError, source: string): string {
    const { line, column, length } = error.location;
    const lines = source.split('\n');
    const errorLine = lines[line - 1];

    if (!errorLine) return error.message;

    const gutter = ` ${line} | `;
    const padding = ' '.repeat(gutter.length + column - 1);
    const carets = '^'.repeat(Math.max(1, length));

    return [
      `${error.type}: ${error.message}`,
      `  at line ${line}, column ${column}`,
      '',
      `${gutter}${errorLine}`,
      `${padding}${carets}`,
    ].join('\n');
  }
}
