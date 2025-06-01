const TYPE = require("../types");

/**
 *
 * @param {Array<{type: string, value: any}} tokens
 * @returns {any}
 */
function parse(tokens) {
  if (!tokens.length) {
    throw new Error("No tokens to parse");
  }

  /**
   *
   * @param {Array<{type: string, value: any}} tokens
   * @returns {any}
   */
  function parseValue(tokens) {
    const token = tokens[tokenIndex];

    switch (token.type) {
      case TYPE.STRING:
      case TYPE.NUMBER:
      case TYPE.BOOLEAN:
      case TYPE.NULL:
        return token.value;
      case TYPE.OPEN_OBJECT:
        return parseObject(tokens, tokenIndex);
      case TYPE.OPEN_ARRAY:
        return parseArray(tokens, tokenIndex);
      default:
        throw new Error(`Unexpected token type: ${token.type}`);
    }
  }

  /**
   *
   * @param {Array<{type: string, value: any}} tokens
   * @returns {Object}
   */
  function parseObject(tokens) {
    const obj = {};
    tokenIndex++;
    let token = tokens[tokenIndex];
    while (token.type !== TYPE.CLOSE_OBJECT) {
      if (token.type === TYPE.STRING) {
        tokenIndex++;
      }
      if (token.type === TYPE.COMMA) {
        tokenIndex++;
      }
      if (token.type === TYPE.COLON) {
        const key = tokens[tokenIndex - 1];
        const value = parseValue(tokens, ++tokenIndex);
        obj[key.value] = value;
        tokenIndex++;
      }

      token = tokens[tokenIndex];
    }
    return obj;
  }

  /**
   *
   * @param {Array<{type: string, value: any}} tokens
   * @returns {Array<any>}
   */
  function parseArray(tokens) {
    const arr = [];
    tokenIndex++;
    let token = tokens[tokenIndex];
    while (token.type !== TYPE.CLOSE_ARRAY) {
      if (token.type === TYPE.COMMA) {
        tokenIndex++;
      } else {
        const value = parseValue(tokens, tokenIndex);
        arr.push(value);
        tokenIndex++;
      }

      token = tokens[tokenIndex];
    }
    return arr;
  }
  let tokenIndex = 0;
  return parseValue(tokens);
}

module.exports = { parse };
