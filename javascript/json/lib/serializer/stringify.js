function stringify(value) {
  switch (typeof value) {
    case "number":
      return String(value);
    case "string":
      return `"${value}"`;
    case "boolean":
      return value ? "true" : "false";
    case "object":
      if (value === null) {
        return "null";
      }
      if (Array.isArray(value)) {
        return `[${value.map(stringify).join(",")}]`;
      }
      const entries = Object.entries(value)
        .map(([key, val]) => `"${key}":${stringify(val)}`)
        .join(",");
      return `{${entries}}`;
  }
}

module.exports = stringify;
