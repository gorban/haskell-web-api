const vscode = require("vscode");
const { runOrmolu } = require("./ormolu");

function activate(context) {
  const provider = vscode.languages.registerDocumentFormattingEditProvider(
    { language: "haskell" },
    {
      async provideDocumentFormattingEdits(document) {
        const executable = vscode.workspace
          .getConfiguration("haskellWebApiOrmolu")
          .get("executable", "ormolu");
        try {
          const formatted = await runOrmolu(executable, document.fileName, document.getText());
          const end = document.lineAt(document.lineCount - 1).range.end;
          return [vscode.TextEdit.replace(new vscode.Range(new vscode.Position(0, 0), end), formatted)];
        } catch (error) {
          const message = error instanceof Error ? error.message : String(error);
          void vscode.window.showErrorMessage(`Ormolu formatting failed: ${message}`);
          return [];
        }
      }
    }
  );
  context.subscriptions.push(provider);
}

function deactivate() {}

module.exports = { activate, deactivate };
