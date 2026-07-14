import { ErrorReporter, InterpreterError } from '../core/errors';
import { toDebugAST } from '../core/parser/debug';
import { Runner } from '../core/runner';
import { startRepl } from './repl';
import * as fs from 'node:fs';

function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    startRepl();
    return;
  }

  const showAST = args.includes('--show-ast');
  const filePath = args.find((arg) => !arg.startsWith('--'));

  if (!filePath) {
    console.error('Error: No file path provided.');
    process.exit(1);
  }

  if (!fs.existsSync(filePath)) {
    console.error(`Error: File not found: ${filePath}`);
    process.exit(1);
  }

  const code = fs.readFileSync(filePath, 'utf-8');
  const runner = new Runner();

  try {
    if (showAST) {
      const program = runner.parse(code);
      console.log(toDebugAST(program));
      return;
    }

    runner.run(code);
  } catch (error) {
    if (error instanceof InterpreterError) {
      console.error(ErrorReporter.report(error, code));
    } else {
      console.error('Unexpected Error:', error instanceof Error ? error.message : String(error));
    }
    process.exit(1);
  }
}

main();
