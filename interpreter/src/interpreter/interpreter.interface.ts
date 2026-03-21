export enum RuntimeType {
  NUMBER = 'NUMBER',
  IDENTIFIER = 'IDENTIFIER',
  VOID = 'VOID',
}

export interface RuntimeValue {
  type: RuntimeType;
  value: any;
}
