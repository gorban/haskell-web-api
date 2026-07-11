const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const os = require("node:os");
const path = require("node:path");
const test = require("node:test");
const { runOrmolu } = require("../ormolu");

async function withFakeOrmolu(body) {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "ormolu-formatter-test-"));
  const executable = path.join(directory, "fake-ormolu.js");
  await fs.writeFile(
    executable,
    [
      "#!/usr/bin/env node",
      "let source = '';",
      "process.stdin.setEncoding('utf8');",
      "process.stdin.on('data', (chunk) => { source += chunk; });",
      "process.stdin.on('end', () => {",
      "  if (source === 'invalid') { process.stderr.write('synthetic formatter failure'); process.exit(7); }",
      "  process.stdout.write(JSON.stringify({ arguments: process.argv.slice(2), source }));",
      "});"
    ].join("\n"),
    { mode: 0o755 }
  );
  try {
    await body(executable);
  } finally {
    await fs.rm(directory, { recursive: true, force: true });
  }
}

test("passes the document path and in-memory source to Ormolu", async () => {
  await withFakeOrmolu(async (executable) => {
    const output = await runOrmolu(executable, "/workspace/Example.hs", "answer=42\n");
    assert.deepEqual(JSON.parse(output), {
      arguments: ["--stdin-input-file", "/workspace/Example.hs"],
      source: "answer=42\n"
    });
  });
});

test("surfaces formatter stderr when Ormolu fails", async () => {
  await withFakeOrmolu(async (executable) => {
    await assert.rejects(
      runOrmolu(executable, "/workspace/Example.hs", "invalid"),
      /synthetic formatter failure/
    );
  });
});

test("surfaces an executable launch failure", async () => {
  await assert.rejects(
    runOrmolu("/definitely/missing/ormolu", "/workspace/Example.hs", "answer = 42\n"),
    /ENOENT/
  );
});
