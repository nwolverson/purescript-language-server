# PureScript language server

Node-based Language Server Protocol server for PureScript based on the PureScript IDE server
(aka psc-ide / `purs ide server`). Used as the [vscode plugin](https://github.com/nwolverson/vscode-ide-purescript)
backend but should be compatible with other Language Server Client implementations.

The language server is a wrapper around the IDE server included as part of the compiler distribution,
providing editing assistance and build features according to support available. This means that
the server will start its own `purs ide server` instance to talk to for the project directory it is started
in.

## Features

- Completion provider
- Definition provider
- Document & workspace symbol providers
- Hover provider
- Code action provider 
  - Compiler fix suggestions for imports/missing types
- Build on save (via IDE server "fast rebuild" facility, certain limitations apply)
  - Provides diagnostics
- Commands
  - Build (full build via `purs compile` / configured build command) - provides diagnostics
  - Case split
  - Add clause
  - Replace suggestion
  - Add completion import
  - Start IDE server
  - Stop IDE server
  - Restart IDE server
- Config
  - `purescript.*`

## Commands

### `purescript.build`

No arguments. Provides diagnostics.

### `purescript.startPscIde`

No arguments. Start IDE server according to configuration.

### `purescript.stopPscIde`

No arguments. Stop running IDE server.

### `purescript.restartPscIde`

No arguments. Stop any running IDE server then start a new one according to configuration.

### `purescript.addCompletionImport`

Arguments: identifier, module, document URI.

### `purescript.replaceSuggestion`

Arguments: document URI, replacement, replacement range.

### `purescript.caseSplit-explicit`

(Used to back the case split command in VS Code UI).

Arguments: document URI, line, character, type.

### `purescript.addClause-explicit`

(Used to back the add clause command in VS Code UI).

Arguments: document URI, line, character.

## Usage

Use as a node module (as in vscode plugin) or via command line `purescript-language-server`. Protocol via command line option:
- `--stdio`
- `--node-ipc`
- `--socket={number}`

## Config

See [config defined in vscode plugin](https://github.com/nwolverson/vscode-ide-purescript/blob/master/package.json).

## Development

See [vscode plugin](https://github.com/nwolverson/vscode-ide-purescript) repo. Common code via
[purescript-ide-purescript-core](https://github.com/nwolverson/purescript-ide-purescript-core).