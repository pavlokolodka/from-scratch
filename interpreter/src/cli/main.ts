import { Runner } from '../core/runner';
import { startRepl } from './repl';
import * as fs from 'node:fs';

function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    startRepl();
    return;
  }

  const filePath = args[0];
  if (!fs.existsSync(filePath)) {
    console.error(`Error: File not found: ${filePath}`);
    process.exit(1);
  }

  const code = fs.readFileSync(filePath, 'utf-8');
  const runner = new Runner();

  try {
    runner.run(code);
  } catch (error) {
    console.error('Error:', error instanceof Error ? error.message : String(error));
    process.exit(1);
  }
}

main();
