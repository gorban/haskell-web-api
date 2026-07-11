const childProcess = require("child_process");

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
    child.on("error", reject);
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

module.exports = { runOrmolu };
