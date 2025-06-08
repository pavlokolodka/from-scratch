const TokenType = require("../token-types");

/**
 *
 * @param {Array<{type: TokenType, value: any}} tokens
 * @returns {any}
 */
function parse(tokens) {
  if (!tokens.length) {
    throw new Error("No tokens to parse");
  }

  /**
   *
   * @param {Array<{type: TokenType, value: any}} tokens
   * @returns {any}
   */
  function parseValue(tokens) {
    const token = tokens[tokenIndex];

    switch (token.type) {
      case TokenType.STRING:
      case TokenType.NUMBER:
      case TokenType.BOOLEAN:
      case TokenType.NULL:
        return token.value;
      case TokenType.OPEN_OBJECT:
        return parseObject(tokens, tokenIndex);
      case TokenType.OPEN_ARRAY:
        return parseArray(tokens, tokenIndex);
      default:
        throw new Error(`Unexpected token type: ${token.type}`);
    }
  }

  /**
   *
   * @param {Array<{type: TokenType, value: any}} tokens
   * @returns {Object}
   */
  function parseObject(tokens) {
    const obj = {};
    tokenIndex++;
    let token = tokens[tokenIndex];
    while (token.type !== TokenType.CLOSE_OBJECT) {
      if (token.type === TokenType.STRING) {
        tokenIndex++;
      }
      if (token.type === TokenType.COMMA) {
        tokenIndex++;
      }
      if (token.type === TokenType.COLON) {
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
   * @param {Array<{type: TokenType, value: any}} tokens
   * @returns {Array<any>}
   */
  function parseArray(tokens) {
    const arr = [];
    tokenIndex++;
    let token = tokens[tokenIndex];
    while (token.type !== TokenType.CLOSE_ARRAY) {
      if (token.type === TokenType.COMMA) {
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

module.exports = parse;
