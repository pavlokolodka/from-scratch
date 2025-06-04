const TYPE = require("../types");

/**
 *
 * @param {string} text
 * @returns {Array<{type: string, value: any}>}
 */
function lexer(text) {
  if (typeof text !== "string" || text.length === 0) {
    return [];
  }

  /**@type {Array<{type: string, value: any}} */
  const tokens = [];
  let charIndex = 0;
  let numberOfRightBrace = 0;
  let numberOfLeftBrace = 0;
  let numberOfRightBracket = 0;
  let numberOfLeftBracket = 0;

  while (charIndex < text.length) {
    let char = text[charIndex];

    if (char === TOKEN.WHITESPACE) {
      charIndex++;
      continue;
    }

    // PRIMITIVES
    if (char === TOKEN.DOUBLE_QUOTE) {
      const value = [];
      charIndex++;
      char = text[charIndex];
      while (char !== TOKEN.DOUBLE_QUOTE) {
        if (charIndex >= text.length) {
          throw new Error("Unterminated string literal");
        }
        value.push(char);
        charIndex++;
        char = text[charIndex];
      }
      tokens.push({ type: TYPE.STRING, value: value.join("") });
      charIndex++;
      continue;
    }
    if (REGEX.NUMBER.test(char)) {
      checkBoolOrNull(tokens, char);
      const value = [char];
      charIndex++;
      char = text[charIndex];
      while (REGEX.NUMBER.test(char)) {
        if (charIndex >= text.length) {
          throw new Error("Unterminated number literal");
        }
        value.push(char);
        charIndex++;
        char = text[charIndex];
      }
      if (Number.isNaN(value.join(""))) {
        throw new Error("Invalid number literal");
      }
      tokens.push({ type: TYPE.NUMBER, value: Number(value.join("")) });
      REGEX.NUMBER.test(char) ? charIndex++ : null;
      continue;
    }
    if (REGEX.TRUE.test(text.slice(charIndex))) {
      checkBoolOrNull(tokens, char);
      tokens.push({ type: TYPE.BOOLEAN, value: true });
      charIndex += TOKEN.TRUE.length;
      continue;
    }
    if (REGEX.FALSE.test(text.slice(charIndex))) {
      checkBoolOrNull(tokens, char);
      tokens.push({ type: TYPE.BOOLEAN, value: false });
      charIndex += TOKEN.FALSE.length;
      continue;
    }
    if (REGEX.NULL.test(text.slice(charIndex))) {
      checkBoolOrNull(tokens, char);
      tokens.push({ type: TYPE.NULL, value: null });
      charIndex += TOKEN.NULL.length;
      continue;
    }
    // OBJECT
    if (char === TOKEN.OPEN_OBJECT) {
      tokens.push({ type: TYPE.OPEN_OBJECT, value: char });
      charIndex++;
      const nextChar = text
        .slice(charIndex)
        .replace(REGEX.ALL_WHITESPACE, "")[0];
      if (nextChar !== TOKEN.DOUBLE_QUOTE && nextChar !== TOKEN.CLOSE_OBJECT) {
        throw new Error(`Unexpected token after '{': ${nextChar}`);
      }
      numberOfLeftBrace++;
      continue;
    }
    if (char === TOKEN.CLOSE_OBJECT) {
      if (!numberOfLeftBrace || numberOfLeftBrace < numberOfRightBrace + 1) {
        throw new Error(`Unexpected close object: ${char}`);
      }

      tokens.push({ type: TYPE.CLOSE_OBJECT, value: char });
      numberOfRightBrace++;
      charIndex++;
      continue;
    }
    if (char === TOKEN.COLON) {
      const previousToken = tokens[tokens.length - 1];
      if (previousToken && previousToken.type !== TYPE.STRING) {
        throw new Error(`Unexpected colon: ${char}`);
      }

      tokens.push({ type: TYPE.COLON, value: char });
      charIndex++;
      continue;
    }
    if (char === TOKEN.COMMA) {
      tokens.push({ type: TYPE.COMMA, value: char });
      const openBrace = tokens.find((el) => el.type === TYPE.OPEN_OBJECT);
      const openBracket = tokens.find((el) => el.type === TYPE.OPEN_ARRAY);
      if (!openBrace && !openBracket) {
        throw new Error(`Unexpected non-whitespace character: ${char}`);
      }

      charIndex++;
      const nextChar = text
        .slice(charIndex)
        .replace(REGEX.ALL_WHITESPACE, "")[0];
      if (
        nextChar == TOKEN.CLOSE_OBJECT ||
        nextChar == TOKEN.CLOSE_ARRAY ||
        nextChar == TOKEN.COMMA ||
        nextChar == TOKEN.COLON
      ) {
        throw new Error(`Unexpected token: ${nextChar}`);
      }
      continue;
    }
    // ARRAY
    if (char === TOKEN.OPEN_ARRAY) {
      tokens.push({ type: TYPE.OPEN_ARRAY, value: char });
      charIndex++;
      const nextChar = text[charIndex];
      if (!nextChar) {
        throw new Error(`Unexpected token after '[': ${nextChar}`);
      }
      numberOfLeftBracket++;
      continue;
    }
    if (char === TOKEN.CLOSE_ARRAY) {
      if (
        !numberOfLeftBracket ||
        numberOfLeftBracket < numberOfRightBracket + 1
      ) {
        throw new Error(`Unexpected close array: ${char}`);
      }

      tokens.push({ type: TYPE.CLOSE_ARRAY, value: char });
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
    (el) => el.type === TYPE.OPEN_OBJECT || el.type === TYPE.OPEN_ARRAY
  );
  if (
    previousToken &&
    (previousToken.type === TYPE.BOOLEAN || previousToken.type === TYPE.NULL) &&
    !isNested
  ) {
    throw new Error(`Unexpected non-whitespace character: ${char}`);
  }
}

const TOKEN = {
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

const REGEX = {
  NUMBER: /^-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/,
  NULL: /^null/,
  TRUE: /^true/,
  FALSE: /^false/,
  ALL_WHITESPACE: /\s+/g,
};

module.exports = { lexer };
