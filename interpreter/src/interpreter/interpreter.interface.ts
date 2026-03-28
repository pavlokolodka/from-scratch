export enum RuntimeType {
  NUMBER = 'NUMBER',
  IDENTIFIER = 'IDENTIFIER',
  VOID = 'VOID',
  FUNCTION = 'FUNCTION',
}

export interface RuntimeValue {
  type: RuntimeType;
  value: any;
}
