export enum RuntimeType {
  NUMBER = 'NUMBER',
}

export interface RuntimeValue {
  type: RuntimeType;
  value: any;
}
