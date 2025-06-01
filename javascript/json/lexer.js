/**
 *
 * @param {string} str
 */
function lexer(str) {
  if (typeof str !== "string" || str.length === 0) {
    return [];
  }

  const tokens = [];
  let charIndex = 0;
  while (charIndex < str.length) {
    let char = str[charIndex];

    if (char === TOKEN.WHITESPACE) {
      charIndex++;
      continue;
    }
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

    throw new Error(`Unexpected character: ${char}`);
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

const TYPE = {
  OPEN_OBJECT: "OPEN_OBJECT",
  CLOSE_OBJECT: "CLOSE_OBJECT",
  OPEN_ARRAY: "OPEN_ARRAY",
  CLOSE_ARRAY: "CLOSE_ARRAY",
  NUMBER: "NUMBER",
  STRING: "STRING",
  NULL: "NULL",
  BOOLEAN: "BOOLEAN",
};

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
