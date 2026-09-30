# VS Code Ormolu formatter development tooling

The formatter VSIX ships only its extension sources and manifest; ESLint is a development-only
dependency used by `tools/check-vscode-ormolu-formatter.sh`. The package keeps ESLint pinned for
reproducible linting and uses supported npm root overrides for vulnerable transitive dependencies
until ESLint's own release range no longer needs them. The same check runs `npm audit
--audit-level=high`, so high and critical advisories in the complete development dependency closure
are a gate rather than informational output.
