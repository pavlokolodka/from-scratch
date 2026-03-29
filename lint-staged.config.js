export default {
  '*.{js,ts,json}': ['biome check --write'],
  'interpreter/**/*.ts': () => 'npm run typecheck --prefix interpreter',
};
