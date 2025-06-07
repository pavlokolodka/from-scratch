/**
 *
 * @param {any} value
 * @returns {string | undefined}
 */
function stringify(value) {
  switch (typeof value) {
    case "number":
      if (!Number.isFinite(value)) {
        return "null";
      }
      return String(value);
    case "bigint":
      throw new Error("Cannot serialize BigInt value");
    case "string":
      return `"${value}"`;
    case "boolean":
      return value ? "true" : "false";
    case "object":
      if (value === null) {
        return "null";
      }
      if (value instanceof Date) {
        return `"${value.toISOString()}"`;
      }
      if (Array.isArray(value)) {
        return stringifyArray(value);
      }
      return stringifyObject(value);
  }
}

function stringifyArray(value) {
  const values = value
    .map((el) => (isInvalidJSONValue(el) ? stringify(null) : stringify(el)))
    .join(",");
  return `[${values}]`;
}

function stringifyObject(value) {
  const entries = Object.entries(value)
    .map(([key, val]) =>
      isInvalidJSONValue(val) ? undefined : `"${key}":${stringify(val)}`
    )
    .filter((el) => el !== undefined)
    .join(",");
  return `{${entries}}`;
}

function isInvalidJSONValue(value) {
  return (
    typeof value === "undefined" ||
    typeof value === "function" ||
    typeof value === "symbol"
  );
}

module.exports = stringify;
