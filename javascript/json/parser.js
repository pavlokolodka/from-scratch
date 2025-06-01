const TYPE = require("./types");

/**
 *
 * @param {Array<{type: string, value: any}} tokens
 * @returns {any}
 */
function parse(tokens) {
  if (!tokens.length) {
    throw new Error("No tokens to parse");
  }

  for (let i = 0; i < tokens.length; i++) {
    const token = tokens[i];
    console.log(tokens, token);
    switch (token.type) {
      case TYPE.STRING:
      case TYPE.NUMBER:
      case TYPE.BOOLEAN:
      case TYPE.NULL:
        return token.value;
      case TYPE.OPEN_OBJECT:
        return parseObject(tokens, i);
      default:
        throw new Error(`Unexpected token type: ${token.type}`);
    }
  }
}

/**
 *
 * @param {Array<{type: string, value: any}} tokens
 * @param {number} tokenIndex
 * @returns {any}
 */
function parseObject(tokens, tokenIndex) {
  const obj = {};
  tokenIndex++;
  let token = tokens[tokenIndex];
  // while (token.type !== TYPE.CLOSE_OBJECT) {
  while (tokens.includes({ type: TYPE.CLOSE_OBJECT })) {
    token = tokens[tokenIndex];
    console.log(token);
    if (token.type === TYPE.STRING) {
      tokenIndex++;
    }
    if (token.type === TYPE.COMMA) {
      tokenIndex++;
    }
    if (token.type === TYPE.COLON) {
      const key = tokens[tokenIndex - 1];
      const value = parse(tokens.slice(tokenIndex + 1));
      obj[key.value] = value;
      const lastRightBraceIndex = tokens.findIndex(
        (el, i) => i > tokenIndex && el.type === TYPE.CLOSE_OBJECT
      );
      console.log(
        "tokenIndex",
        tokenIndex,
        "lastRightBraceIndex",
        lastRightBraceIndex
      );
      tokenIndex = lastRightBraceIndex;
    }
  }
  console.log("ob", obj);
  return obj;
}

module.exports = { parse };
