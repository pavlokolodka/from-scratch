import { stringifyOutput } from '../core/interpreter/builtins';
import { Runner } from '../core/runner';
import * as readline from 'node:readline';

export function startRepl() {
  const runner = new Runner();

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: '>> ',
  });

  console.log('Interpreter REPL - Type "exit" to quit');
  rl.prompt();

  rl.on('line', (line) => {
    const input = line.trim();

    if (input === 'exit') {
      rl.close();
      return;
    }

    if (input) {
      try {
        const result = runner.run(input);
        console.log(stringifyOutput(result));
      } catch (error) {
        console.error('Error:', error instanceof Error ? error.message : String(error));
      }
    }

    rl.prompt();
  }).on('close', () => {
    console.log('Exiting REPL');
    process.exit(0);
  });
}
