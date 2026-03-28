export enum RuntimeType {
  NUMBER = 'NUMBER',
  IDENTIFIER = 'IDENTIFIER',
  VOID = 'VOID',
  FUNCTION = 'FUNCTION',
  RETURN = 'RETURN',
}

export interface RuntimeValue {
  type: RuntimeType;
  value: any;
}
