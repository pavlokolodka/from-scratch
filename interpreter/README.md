# Interpreter from Scratch

A custom programming language interpreter implemented in TypeScript.

## Usage

### Run a file
```bash
npm start <path-to-file>
```

### Start REPL
```bash
npm start
```

## Debug Flags

### `--show-ast`
Prints the Abstract Syntax Tree (AST) of the program instead of executing it.
```bash
npm start <path-to-file> -- --show-ast
```

## TODO
- Implement LSP: [Language Server Extension Guide](https://code.visualstudio.com/api/language-extensions/language-server-extension-guide)
