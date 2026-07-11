const childProcess = require("child_process");
const vscode = require("vscode");

function runOrmolu(executable, fileName, source) {
  return new Promise((resolve, reject) => {
    const child = childProcess.spawn(executable, ["--stdin-input-file", fileName], {
      stdio: ["pipe", "pipe", "pipe"]
    });
    let stdout = "";
    let stderr = "";

    child.stdout.setEncoding("utf8");
    child.stderr.setEncoding("utf8");
    child.stdout.on("data", (chunk) => { stdout += chunk; });
    child.stderr.on("data", (chunk) => { stderr += chunk; });
    child.on("error", (error) => reject(error));
    child.on("close", (code) => {
      if (code === 0) {
        resolve(stdout);
      } else {
        reject(new Error(stderr || `${executable} exited with status ${code}`));
      }
    });
    child.stdin.end(source);
  });
}

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
