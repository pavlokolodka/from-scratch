# Language Technical Documentation

This document provides a comprehensive overview of the custom programming language implemented in this interpreter.

## Table of Contents
1. [Data Types](#data-types)
2. [Variables and Constants](#variables-and-constants)
3. [Comments](#comments)
4. [Operators](#operators)
5. [Control Flow](#control-flow)
6. [Functions](#functions)
7. [Built-in Functions](#built-in-functions)
8. [Error Handling and Exceptions](#error-handling-and-exceptions)
9. [Truthiness](#truthiness)

---

## Data Types

The language supports the following fundamental data types:

| Type | Description | Example |
| :--- | :--- | :--- |
| **Number** | 64-bit floating-point numbers. | `42`, `3.14` |
| **String** | Double-quoted sequences of characters. | `"Hello, World!"` |
| **Boolean** | Logical values. | `true`, `false` |
| **Null** | Represents the absence of a value. | `nil` |
| **Array** | Ordered, mutable collections of values. | `[1, "two", true]` |
| **Function** | First-class objects that can be assigned and passed. | `fn(x) { return x * x; }` |

---

## Variables and Constants

The language provides two ways to declare identifiers:

### `let` (Mutable)
Declares a variable that can be re-assigned later.
```javascript
let count = 1;
count = 2; // Valid
```

### `const` (Immutable)
Declares a constant that cannot be re-assigned. Attempting to re-assign a `const` will trigger an `IMMUTABLE` error.
```javascript
const pi = 3.14;
pi = 3; // Error: assignment to constant variable
```

---

## Comments

The language supports single-line comments starting with `//`. Anything following `//` on the same line is ignored by the interpreter.

```javascript
// This is a full-line comment
let x = 10; // This is a trailing comment
```

---

## Operators

### Arithmetic Operators
- `+` : Addition (Numbers only)
- `-` : Subtraction
- `*` : Multiplication
- `/` : Division (Throws `RuntimeError` on division by zero)

*Note: String concatenation is not supported via operators but can be achieved using the `push` built-in function.*

### Comparison Operators
- `==` : Equality
- `!=` : Inequality
- `<`  : Less than
- `>`  : Greater than
- `<=` : Less than or equal to
- `>=` : Greater than or equal to

### Logical and Unary Operators
- `!`  : Logical NOT (Returns boolean)
- `!!` : Double bang (Converts value to its truthy boolean representation)
- `-`  : Unary minus (Negates a number)

### Indexing
- `[]` : Access elements in an array by zero-based index.
```javascript
let arr = [10, 20, 30];
print(arr[1]); // 20
```

---

## Control Flow

### Conditionals
Supports `if`, `elif`, and `else` blocks.
```javascript
if (x > 10) {
    print("Large");
} elif (x > 5) {
    print("Medium");
} else {
    print("Small");
}
```

### Loops
Supports `while` loops.
```javascript
let i = 0;
while (i < 5) {
    print(i);
    i = i + 1;
}
```

### Loop Control
- `stop` : Immediately exits the current loop (similar to `break`).
```javascript
while (true) {
    if (ready) { stop; }
}
```

---

## Functions

Functions are declared using the `fn` keyword. They support parameters, recursion, and closures.

```javascript
fn fib(n) {
    if (n <= 1) { return n; }
    return fib(n - 1) + fib(n - 2);
}

let result = fib(10);
```

- `return` : Exits the function and returns a value. If no value is specified, it returns `nil` (internally `void` converted).

---

## Built-in Functions

The following functions are available in the global scope:

| Function | Description |
| :--- | :--- |
| `print(...args)` | Outputs the string representation of arguments to the console. |
| `len(val)` | Returns the length of a string or an array. |
| `str(val)` | Converts a value to its string representation. |
| `type(val)` | Returns a string representing the type of the value (e.g., `"NUMBER"`, `"ARRAY"`). |
| `push(target, val)` | Appends `val` to an array or string `target`. Returns the new length. |
| `num(val)` | Converts a string or boolean to a number. |

---

## Error Handling and Exceptions

Errors are categorized to provide clear feedback during different stages of execution:

| Error Type | Description |
| :--- | :--- |
| **LEXICAL** | Occurs during tokenization (e.g., unclosed string quotes, illegal characters). |
| **PARSE** | Occurs when the input does not conform to the language grammar. |
| **RUNTIME** | Occurs during execution (e.g., division by zero, index out of bounds). |
| **SCOPE** | Occurs when trying to access an undefined variable. |
| **IMMUTABLE** | Occurs when attempting to re-assign a value to a `const` declaration. |

---

## Truthiness

The language uses the following rules to determine if a value is "truthy" in conditions:

- `nil` is **falsy**.
- `false` is **falsy**.
- `0` (Number) is **falsy**.
- `""` (Empty string) is **falsy**.
- `[]` (Empty array) is **falsy**.
- Everything else is **truthy**.
