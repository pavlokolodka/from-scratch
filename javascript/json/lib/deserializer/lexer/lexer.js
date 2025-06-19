const TokenType = require("../token-types");

/**
 *
 * @param {string} text
 * @returns {Array<{type: TokenType, value: any}>}
 */
function lexer(text) {
  if (typeof text !== "string" || text.length === 0) {
    return [];
  }

  /**@type {Array<{type: TokenType, value: any}} */
  const tokens = [];
  let charIndex = 0;
  let numberOfRightBrace = 0;
  let numberOfLeftBrace = 0;
  let numberOfRightBracket = 0;
  let numberOfLeftBracket = 0;

  while (charIndex < text.length) {
    let char = text[charIndex];

    if (char === Char.WHITESPACE) {
      charIndex++;
      continue;
    }

    // PRIMITIVES
    if (char === Char.DOUBLE_QUOTE) {
      const value = [];
      charIndex++;
      char = text[charIndex];
      while (char !== Char.DOUBLE_QUOTE) {
        if (charIndex >= text.length) {
          throw new Error("Unterminated string literal");
        }

        value.push(char);
        charIndex++;
        char = text[charIndex];
      }

      tokens.push({ type: TokenType.STRING, value: value.join("") });
      charIndex++;
      continue;
    }
    if (RegEx.NUMBER_START.test(char)) {
      checkBoolOrNull(tokens, char);
      const value = [char];
      charIndex++;
      char = text[charIndex];
      while (RegEx.NUMBER.test(char)) {
        value.push(char);
        charIndex++;
        char = text[charIndex];
      }

      if (
        Number.isNaN(Number(value.join(""))) ||
        (value.length > 2 &&
          ((value[0] === "0" && value[1] !== ".") ||
            value[value.length - 1] === "."))
      ) {
        throw new Error("Invalid number literal");
      }

      tokens.push({ type: TokenType.NUMBER, value: Number(value.join("")) });
      RegEx.NUMBER.test(char) ? charIndex++ : null;
      continue;
    }
    if (RegEx.TRUE.test(text.slice(charIndex))) {
      checkBoolOrNull(tokens, char);
      tokens.push({ type: TokenType.BOOLEAN, value: true });
      charIndex += Char.TRUE.length;
      continue;
    }
    if (RegEx.FALSE.test(text.slice(charIndex))) {
      checkBoolOrNull(tokens, char);
      tokens.push({ type: TokenType.BOOLEAN, value: false });
      charIndex += Char.FALSE.length;
      continue;
    }
    if (RegEx.NULL.test(text.slice(charIndex))) {
      checkBoolOrNull(tokens, char);
      tokens.push({ type: TokenType.NULL, value: null });
      charIndex += Char.NULL.length;
      continue;
    }
    // OBJECT
    if (char === Char.OPEN_OBJECT) {
      charIndex++;
      const nextChar = text
        .slice(charIndex)
        .replace(RegEx.ALL_WHITESPACES, "")[0];
      if (nextChar !== Char.DOUBLE_QUOTE && nextChar !== Char.CLOSE_OBJECT) {
        throw new Error(`Unexpected token after '{': ${nextChar}`);
      }

      tokens.push({ type: TokenType.OPEN_OBJECT, value: char });
      numberOfLeftBrace++;
      continue;
    }
    if (char === Char.CLOSE_OBJECT) {
      if (!numberOfLeftBrace || numberOfLeftBrace < numberOfRightBrace + 1) {
        throw new Error(`Unexpected close object: ${char}`);
      }

      tokens.push({ type: TokenType.CLOSE_OBJECT, value: char });
      numberOfRightBrace++;
      charIndex++;
      continue;
    }
    if (char === Char.COLON) {
      const previousToken = tokens[tokens.length - 1];
      if (previousToken && previousToken.type !== TokenType.STRING) {
        throw new Error(`Unexpected colon: ${char}`);
      }

      tokens.push({ type: TokenType.COLON, value: char });
      charIndex++;
      continue;
    }
    if (char === Char.COMMA) {
      const openBrace = tokens.find((el) => el.type === TokenType.OPEN_OBJECT);
      const openBracket = tokens.find((el) => el.type === TokenType.OPEN_ARRAY);
      if (!openBrace && !openBracket) {
        throw new Error(`Unexpected non-whitespace character: ${char}`);
      }

      charIndex++;
      const nextChar = text
        .slice(charIndex)
        .replace(RegEx.ALL_WHITESPACES, "")[0];
      if (
        nextChar == Char.CLOSE_OBJECT ||
        nextChar == Char.CLOSE_ARRAY ||
        nextChar == Char.COMMA ||
        nextChar == Char.COLON
      ) {
        throw new Error(`Unexpected token: ${nextChar}`);
      }

      tokens.push({ type: TokenType.COMMA, value: char });
      continue;
    }
    // ARRAY
    if (char === Char.OPEN_ARRAY) {
      charIndex++;
      const nextChar = text[charIndex];
      if (!nextChar) {
        throw new Error(`Unexpected token after '[': ${nextChar}`);
      }

      tokens.push({ type: TokenType.OPEN_ARRAY, value: char });
      numberOfLeftBracket++;
      continue;
    }
    if (char === Char.CLOSE_ARRAY) {
      if (
        !numberOfLeftBracket ||
        numberOfLeftBracket < numberOfRightBracket + 1
      ) {
        throw new Error(`Unexpected close array: ${char}`);
      }

      tokens.push({ type: TokenType.CLOSE_ARRAY, value: char });
      numberOfRightBracket++;
      charIndex++;
      continue;
    }

    throw new Error(`Unexpected token: ${char}`);
  }

  if (numberOfLeftBrace !== numberOfRightBrace) {
    throw new Error(`Expected ',' or '}' after property value`);
  }
  if (numberOfLeftBracket !== numberOfRightBracket) {
    throw new Error(`Expected ',' or ']' after array element`);
  }

  return tokens;
}

function checkBoolOrNull(tokens, char) {
  const previousToken = tokens[tokens.length - 1];
  const isNested = tokens.find(
    (el) =>
      el.type === TokenType.OPEN_OBJECT || el.type === TokenType.OPEN_ARRAY
  );
  if (
    previousToken &&
    (previousToken.type === TokenType.BOOLEAN ||
      previousToken.type === TokenType.NULL) &&
    !isNested
  ) {
    throw new Error(`Unexpected non-whitespace character: ${char}`);
  }
}

const Char = {
  OPEN_OBJECT: "{",
  CLOSE_OBJECT: "}",
  OPEN_ARRAY: "[",
  CLOSE_ARRAY: "]",
  COMMA: ",",
  COLON: ":",
  DOUBLE_QUOTE: '"',
  WHITESPACE: " ",
  NULL: "null",
  TRUE: "true",
  FALSE: "false",
};

const RegEx = {
  NUMBER_START: /\d|-/,
  NUMBER: /^(\d|\.|[eE])/,
  NULL: /^null/,
  TRUE: /^true/,
  FALSE: /^false/,
  ALL_WHITESPACES: /\s+/g,
};

module.exports = lexer;
