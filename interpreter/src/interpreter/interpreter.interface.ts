export enum RuntimeType {
  NUMBER = 'NUMBER',
  STRING = 'STRING',
  IDENTIFIER = 'IDENTIFIER',
  VOID = 'VOID',
  FUNCTION = 'FUNCTION',
  ARRAY = 'ARRAY',
  RETURN = 'RETURN',
}

export interface RuntimeValue {
  type: RuntimeType;
  value: any;
}
