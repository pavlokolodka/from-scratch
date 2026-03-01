# Language Specification

## 1. Overview
This document outlines the grammar and semantics for a custom, dynamically typed, interpreted programming language. It uses a familiar C-style syntax for blocks and control flow but simplifies keywords and structures to facilitate clean parsing and evaluation.

## 2. Data Types and Data Structures
The language supports the following primitive and composite data types:

* **Nil:** Represents the intentional absence of a value. 
  * Syntax: `nil`
* **Characters:** A single character, enclosed in single quotes.
  * Syntax: `'a'`, `'\n'`
* **Strings:** A sequence of characters, enclosed in double quotes.
  * Syntax: `"Hello, World!"`
* **Arrays:** Ordered, comma-separated lists of expressions enclosed in square brackets.
  * Syntax: `[1, 2, "three", nil]`
* **Objects:** Key-value dictionaries enclosed in curly braces. Keys are typically identifiers or strings.
  * Syntax: `{ name: "Alice", age: 25 }`

## 3. Variables
Variables are block-scoped and require explicit declaration.

* **`let`**: Declares a mutable variable that can be reassigned.
  ```javascript
  let counter = 0;
  counter = counter + 1;
  ```
* **`const`**: Declares an immutable variable. It must be initialized immediately and cannot be reassigned.
  ```javascript
  const pi = 3.14;
  ```

## 4. Control Flow
Control flow uses C-style curly braces `{}` to define blocks and scopes.

### Conditionals (`if` / `elif` / `else`)
Supports up to two conditional branches and one default fallback.

```javascript
if (x > 10) {
    print("High");
} elif (x > 5) {
    print("Medium");
} else {
    print("Low");
}
```

### Loops (`while`)
The sole looping construct. Executes as long as the condition evaluates to true.

```javascript
let i = 0;
while (i < 10) {
    print(i);
    i = i + 1;
}
```

## 5. Functions
Functions are declared using the `fn` keyword, followed by an identifier, a parameter list in parentheses, and a block body.

```javascript
fn greet(name) {
    print("Hello, " + name);
}
```

## 6. Built-in Functions

### `print(value)`
Outputs the given value to the standard output.

```javascript
print("Hello, world!");
let x = 10;
print(x);
```
