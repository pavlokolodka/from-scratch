const TYPE = require("./types");

/**
 *
 * @param {string} text
 * @returns {Array<{type: string, value: any}>}
 */
function lexer(text) {
  if (typeof text !== "string" || text.length === 0) {
    return [];
  }
  const str = text.replace(/\s+/g, "");

  /**@type {Array<{type: string, value: any}} */
  const tokens = [];
  let charIndex = 0;
  let numberOfRightBrace = 0;
  let numberOfLeftBrace = 0;

  while (charIndex < str.length) {
    let char = str[charIndex];

    // PRIMITIVES
    if (char === TOKEN.DOUBLE_QUOTE) {
      const value = [];
      charIndex++;
      char = str[charIndex];
      while (char !== TOKEN.DOUBLE_QUOTE) {
        if (charIndex >= str.length) {
          throw new Error("Unterminated string literal");
        }
        value.push(char);
        charIndex++;
        char = str[charIndex];
      }
      tokens.push({ type: TYPE.STRING, value: value.join("") });
      charIndex++;
      continue;
    }
    if (VALUE_REGEX.NUMBER.test(char)) {
      checkBoolOrNull(tokens, char);
      const value = [char];
      charIndex++;
      char = str[charIndex];
      while (VALUE_REGEX.NUMBER.test(char)) {
        if (charIndex >= str.length) {
          throw new Error("Unterminated number literal");
        }
        value.push(char);
        charIndex++;
        char = str[charIndex];
      }
      if (Number.isNaN(value.join(""))) {
        throw new Error("Invalid number literal");
      }
      tokens.push({ type: TYPE.NUMBER, value: Number(value.join("")) });
      VALUE_REGEX.NUMBER.test(char) ? charIndex++ : null;
      continue;
    }
    if (VALUE_REGEX.TRUE.test(str.slice(charIndex))) {
      checkBoolOrNull(tokens, char);
      tokens.push({ type: TYPE.BOOLEAN, value: true });
      charIndex += TOKEN.TRUE.length;
      continue;
    }
    if (VALUE_REGEX.FALSE.test(str.slice(charIndex))) {
      checkBoolOrNull(tokens, char);
      tokens.push({ type: TYPE.BOOLEAN, value: false });
      charIndex += TOKEN.FALSE.length;
      continue;
    }
    if (VALUE_REGEX.NULL.test(str.slice(charIndex))) {
      checkBoolOrNull(tokens, char);
      tokens.push({ type: TYPE.NULL, value: null });
      charIndex += TOKEN.NULL.length;
      continue;
    }
    // OBJECT
    if (char === TOKEN.OPEN_OBJECT) {
      tokens.push({ type: TYPE.OPEN_OBJECT, value: char });
      charIndex++;
      const nextChar = str[charIndex];
      if (nextChar !== TOKEN.DOUBLE_QUOTE && nextChar !== TOKEN.CLOSE_OBJECT) {
        throw new Error(`Unexpected character after '{': ${nextChar}`);
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
      charIndex++;
      const nextChar = str[charIndex];
      if (nextChar == TOKEN.CLOSE_OBJECT && nextChar == TOKEN.CLOSE_ARRAY) {
        throw new Error(`Unexpected trailing comma: ${nextChar}`);
      }
      continue;
    }
    // TODO: ARRAY
    throw new Error(`Unexpected character: ${char}`);
  }

  if (numberOfLeftBrace !== numberOfRightBrace) {
    throw new Error(`Expected ',' or '}' after property value`);
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

const VALUE_REGEX = {
  NUMBER: /^-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?/,
  NULL: /^null/,
  TRUE: /^true/,
  FALSE: /^false/,
  WHITESPACE: /^\s+/,
};

module.exports = { lexer };
