module.exports = {
  root: true,
  parser: '@typescript-eslint/parser',
  plugins: [
    '@typescript-eslint',
    'import',
  ],
  extends: [
    'eslint:recommended',
    'plugin:react/recommended',
    'plugin:react-hooks/recommended',
    'plugin:@typescript-eslint/eslint-recommended',
    'plugin:@typescript-eslint/recommended',
  ],
  parserOptions: {
    project: './tsconfig.json',
  },
  settings: {
    // Electron is in devDependencies because of its special build system
    "import/core-modules": ['electron', 'react-hot-loader']
  },
  rules: {
    // --- Style rules ---

    // Force code to 80 columns, but for trailing comments
    "max-len": ["error", { "code": 80, "ignoreTrailingComments": true, }],
    // Camelcase identifers
    "camelcase": "error",
    // Do not allow functions without return type, except function expressions (arrow functions)
    "@typescript-eslint/explicit-function-return-type": [
      "error",
      {
        allowExpressions: true,
        allowTypedFunctionExpressions: true,
        allowConciseArrowFunctionExpressionsStartingWithVoid: true
      }
    ],
    // Force single class member per line
    "lines-between-class-members": [
      "error", "always", { "exceptAfterSingleLine": true }
    ],
    // Allow infinite loops but still disallow constant if-then-else
    "no-constant-condition": ["error", { "checkLoops": false }],
    // Prefer const including when destructuring when _all_ destructured values
    // may be const
    "prefer-const": [
      "error",
      { "destructuring": "all", "ignoreReadBeforeAssign": false }
    ],
    // Disallow the use of console.* to prevent the release of code producing
    // various debuging messages
    "no-console": "error",
    // Enforce consistent spacing after the // or /* in a comment
    "spaced-comment": ["error", "always"],
    // Enforce consistent spacing before and after the arrow in arrow functions
    "arrow-spacing": "error",
    // Enforce spaces inside of blocks after opening block and
    // before closing block
    "block-spacing": "error",
    // Enforce consistent spacing before and after commas
    "comma-spacing": "error",
    // Enforce consistent newlines before and after dots
    "dot-location": ["error", "property"],
    // Require newline at the end of files
    "eol-last": "error",
    // Disallow spacing between function identifiers and their invocations
    "func-call-spacing": "error",
    // Enforce consistent spacing between keys and values in object literal
    // properties
    "key-spacing": "error",
    // Disallow trailing whitespace at the end of lines
    "no-trailing-spaces": "error",
    // Enforce consistent spacing inside braces
    "object-curly-spacing": ["error", "always"],
    // Enforce consistent spacing before and after semicolons
    "semi-spacing": "error",
    // Enforce location of semicolons
    "semi-style": "error",
    // Enforce spacing around colons of switch statements
    "switch-colon-spacing": "error",
    // Enforce consistent spacing before blocks
    "space-before-blocks": "error",
    // Enforce consistent spacing before function definition opening parenthesis
    "space-before-function-paren": [
      "error",
      {"anonymous": "always", "named": "never", "asyncArrow": "always"}
    ], 

    // --- Safety rules ---

    // Requires semicolon at the end of each statement to prevent
    // misinterpretetion errors
    "semi": "error",
    // Be more strict on usage of useMemo and useRef
    "react-hooks/exhaustive-deps": "error",
    // Requires '+' to be applied on 2 numbers or 2 strings only
    "@typescript-eslint/restrict-plus-operands": "error",
    // Only use type safe comparison === as == between distinct type is not so obvious
    "eqeqeq": "error",
    // Disallow unused variables except variables starting with _
    "no-unused-vars": "off", // Must be turned off for the following typescript rule
    "@typescript-eslint/no-unused-vars": [
      "error", {
        "vars": "local",
        "ignoreRestSiblings": true, // Useful to remove properties using rest properties
        "varsIgnorePattern": "^_",
        "argsIgnorePattern": "^_"
      }
    ],
    // Disallow the use of var in favor of let and const
    "no-var": "error",
    // Disallow explicit any types ; common workaround includes using 'unknown'
    // when the type can't be infered
    "@typescript-eslint/no-explicit-any": "error",
  }
};
