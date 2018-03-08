# PureScript language server

Node-based Language Server Protocol server for PureScript based on the PureScript IDE server
(aka psc-ide / `purs ide server`). Used as the [vscode plugin](https://github.com/nwolverson/vscode-ide-purescript)
backend but should be compatible with other Language Server Client implementations.

The language server is a wrapper around the IDE server included as part of the compiler distribution,
providing editing assistance and build features according to support available. This means that
the server will start its own `purs ide server` instance to talk to for the project directory it is started
in.

## Features

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

### `purescript.startPscIde`

No arguments. Start IDE server according to configuration.

### `purescript.stopPscIde`

No arguments. Stop running IDE server.

### `purescript.restartPscIde`

No arguments. Stop any running IDE server then start a new one according to configuration.

### `purescript.addCompletionImport`

Arguments: identifier, module, document URI.

### `purescript.addModuleImport`

Arguments: module, qualifier, document URI.

### `purescript.getAvailableModules`

No arguments. Get list of available modules.

### `purescript.replaceSuggestion`

Arguments: document URI, replacement, replacement range.

### `purescript.search`

Flex search for identifier.

Arguments: search text.

### `purescript.fixTypo`

Arguments: document URI, line, character.

### `purescript.caseSplit-explicit`

(Used to back the case split command in VS Code UI).

Arguments: document URI, line, character, type.

### `purescript.addClause-explicit`

(Used to back the add clause command in VS Code UI).

Arguments: document URI, line, character.

### `purescript.typedHole-explicit`

(Used to back the `purescript.typedHole` code action triggered in the VS Code UI)

Arguments: hole name, document URI, hole range, `PscIde.Command.TypeInfo` of chosen replacement option

## Usage

Use as a node module (as in vscode plugin) or via command line `purescript-language-server`. Protocol via command line option:
- `--stdio`
- `--node-ipc`
- `--socket={number}`

## Config

See [config defined in vscode plugin](https://github.com/nwolverson/vscode-ide-purescript/blob/master/package.json).

## Development

To develop (rather than use) this language server

1. Clone this repo and `npm install`
2. Make changes and `npm run build`
3. Ensure the built module is picked up by your editor

For 3, if the editor integrates using the node module rather than standalone binary, I suggest using `npm link` - this will work for atom and vscode at least.

For atom, clone `atom-ide-purescript` and:

1. In `purescript-language-server` run `npm link`, in `atom-ide-purescript` run `npm link purescript-language-server`
2. In `atom-ide-purescript` run `apm link` to pick up local changes
3. In `atom-ide-purescript`, run `npm run bundle` to build the plugin itself
4. Reload any atom window to pick up changes

For vscode, clone `vscode-ide-purescript` and:

1. In `purescript-language-server` run `npm link`, in `vscode-ide-purescript` run `npm link purescript-language-server`
2. Open `vscode-ide-purescript` in vscode and hit F5 to "launch extension"
3. Use the newly launched Extension Development Host to test language server changes

See [vscode plugin](https://github.com/nwolverson/vscode-ide-purescript) repo, [atom plugin](https://github.com/nwolverson/atom-ide-purescript). Common code via
[purescript-ide-purescript-core](https://github.com/nwolverson/purescript-ide-purescript-core).
