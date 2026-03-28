import { Lexer } from '../lexer/lexer';
import { Parser } from '../parser/parser';
import * as readline from 'node:readline';

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: '>> ',
});

console.log('Interpreters Repl - Type "exit" to quit');
rl.prompt();

rl.on('line', (line) => {
  const input = line.trim();

  if (input === 'exit') {
    rl.close();
    return;
  }

  if (input) {
    try {
      const lexer = new Lexer(input);
      const parser = new Parser(lexer.tokenize());
      const program = parser.parse();

      // Display the AST in a readable JSON format, hiding the 'token' metadata
      console.log(
        JSON.stringify(program, (key, value) => (key === 'token' ? undefined : value), 2),
      );
    } catch (error) {
      console.error('Error:', error instanceof Error ? error.message : String(error));
    }
  }

  rl.prompt();
}).on('close', () => {
  console.log('Exiting Repl');
  process.exit(0);
});
