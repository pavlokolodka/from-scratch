export enum RuntimeType {
  NUMBER = 'NUMBER',
  VOID = 'VOID',
}

export interface RuntimeValue {
  type: RuntimeType;
  value: any;
}
