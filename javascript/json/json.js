const { lexer } = require("./lib/lexer");
const { parse: _parse } = require("./lib/parser");

/**
 * Parses a JSON string and converts it into a JavaScript object or value.
 *
 * @param {string} text - The JSON string to parse.
 * @returns {*} The JavaScript object or value resulting from parsing the JSON string.
 */
function parse(text) {
  const tokens = lexer(text);
  return _parse(tokens);
}

module.exports = { parse };
