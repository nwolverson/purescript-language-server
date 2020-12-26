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
- Formatting provider
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

## Usage

This LSP implmementation is consumed by vscode and Atom plugins as a node module, and bundled along with those plugins.

To use with another LSP client, you will want to install this either globally or locally for `npm`, e.g.

```
npm i -g purescript-language-server
```
And then use the resulting executable, e.g. `purescript-language-server --stdio`.

This language server is based on vscode-languageserver-node which means it should support `--stdio`, `--socket=[number]`, `--node-ipc` or `--pipe` methods of communication, see [vscode-languageserver-node](https://github.com/Microsoft/vscode-languageserver-node) for details.

### VSCode 

Use [vscode-ide-purescript](https://github.com/nwolverson/vscode-ide-purescript).

### Atom

Use [atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript).

### Vim/LanguageClient_neovim

Use [vimmer-ps](https://github.com/sriharshachilakapati/vimmer-ps).

### Vim/CoC

Configuration with [coc.nvim](https://github.com/neoclide/coc.nvim/wiki/Language-servers#purescript):

```jsonc
    "purescript": {
      "command": "purescript-language-server",
      "args": ["--stdio"],
      "filetypes": ["purescript"],
      "trace.server": "off",
      "rootPatterns": ["bower.json", "psc-package.json", "spago.dhall"],
      "initializationOptions" : { 
        "addNpmPath"     : true,
        "addSpagoSources": true,
        "buildCommand"   : "spago build --purs-args --json-errors",
        "autocompleteAllModules": true,
        "autocompleteAddImport": true,
        "fastRebuild": true
      },
      "settings": {
        "purescript": {
        }
      }
    }
```

### Other clients

Config may be supplied via client-push on startup (`workspace.didChangeConfiguration`), server-request (`workspace.configuration`), or at last resort by JSON object on the command line with `--config` option.

## Config
See [config defined in vscode plugin](https://github.com/nwolverson/vscode-ide-purescript/blob/master/package.json).

### Usage with alternate backends

When using the language server together with alternate backends, the only requirement is to stop `purs ide server` from attempting to generate JS when rebuilding, this is done via the config

```json
"purescript.codegenTargets": [ "corefn" ]
```

(and you should make sure the build command is in accordance with that, if used, eg specify `backend` in `spago` config).

## Commands

Various commands are provided. Some are triggered via completion etc, some must be called explicitly from a LSP client.

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

1. Run `npm install`
1. In `purescript-language-server` run `npm link`, in `vscode-ide-purescript` run `npm link purescript-language-server`
1. Open `vscode-ide-purescript` in vscode (`code .`) and hit F5 to "launch extension"
1. Use the newly launched Extension Development Host to test language server changes

See [vscode plugin](https://github.com/nwolverson/vscode-ide-purescript) repo, [atom plugin](https://github.com/nwolverson/atom-ide-purescript). Common code via
[purescript-ide-purescript-core](https://github.com/nwolverson/purescript-ide-purescript-core).
