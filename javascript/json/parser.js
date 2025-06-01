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

  for (const token of tokens) {
    switch (token.type) {
      case TYPE.STRING:
      case TYPE.NUMBER:
      case TYPE.BOOLEAN:
      case TYPE.NULL:
        return token.value;
      default:
        throw new Error(`Unexpected token type: ${token.type}`);
    }
  }
}

module.exports = { parse };
