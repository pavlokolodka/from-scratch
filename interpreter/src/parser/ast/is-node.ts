import type { Node, NodeKindMap } from './ast.interface';

export function isNode<K extends keyof NodeKindMap>(ast: Node, k: K): ast is NodeKindMap[K] {
  return ast.kind === k;
}
