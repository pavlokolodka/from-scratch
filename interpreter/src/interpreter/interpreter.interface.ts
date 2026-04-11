export enum RuntimeType {
  NUMBER = 'NUMBER',
  STRING = 'STRING',
  BOOLEAN = 'BOOLEAN',
  IDENTIFIER = 'IDENTIFIER',
  NULL = 'NULL',
  VOID = 'VOID',
  FUNCTION = 'FUNCTION',
  ARRAY = 'ARRAY',
  RETURN = 'RETURN',
}

export interface RuntimeValue {
  type: RuntimeType;
  value: any;
}
