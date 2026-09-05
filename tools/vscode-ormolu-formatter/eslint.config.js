module.exports = [
  {
    files: ["*.js", "test/*.test.js"],
    languageOptions: {
      ecmaVersion: 2022,
      sourceType: "commonjs",
      globals: {
        Buffer: "readonly",
        Error: "readonly",
        JSON: "readonly",
        Promise: "readonly",
        String: "readonly",
        console: "readonly",
        module: "readonly",
        process: "readonly",
        require: "readonly"
      }
    },
    rules: {
      "no-unused-vars": "error",
      "no-undef": "error",
      "no-useless-catch": "error"
    }
  }
];
