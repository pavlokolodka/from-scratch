/** @typedef {object} TokenType */
const TokenType = {
  OPEN_OBJECT: "OPEN_OBJECT",
  CLOSE_OBJECT: "CLOSE_OBJECT",
  COLON: "COLON",
  COMMA: "COMMA",
  OPEN_ARRAY: "OPEN_ARRAY",
  CLOSE_ARRAY: "CLOSE_ARRAY",
  NUMBER: "NUMBER",
  STRING: "STRING",
  NULL: "NULL",
  BOOLEAN: "BOOLEAN",
};

module.exports = TokenType;
