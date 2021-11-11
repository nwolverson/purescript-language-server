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

#### Version support policy

PureScript compiler version support is as follows:

* The current minor version of the compiler is supported at all patch versions (e.g. 0.14.xx)
* The previous minor version of the compiler is supported at the latest patch version (e.g. 0.13.8) for new functionality, and where possible all patch versions for existing functionality
* Any older compiler versions are not officially supported - they may continue to work and will not be intentionally broken, but no particular effort will be made for continued support in the face of API changes

### Formatting provider

The `purescript-language-server` comes with built-in support for [purty](https://gitlab.com/joneshf/purty) for formatting PureScript code. `purty` itself is not bundled with `purescript-language-server`, so you must either install it globally (e.g. `npm install -g purty`), or locally in your project (e.g. `npm install --save-dev purty`). When a formatting operation is requested via a language server command, `purescript-language-server` will attempt to find `purty` in your `$PATH`. If you're using a local `purty` install, you can configure the language server to include your local `npm` install path (i.e. `./node_modules/.bin`) using the `purescript.addNpmPath` setting. See below for information on configuring the language server for different editors.

### VSCode 

Use [vscode-ide-purescript](https://github.com/nwolverson/vscode-ide-purescript).

### Atom

Use [atom-ide-purescript](https://github.com/nwolverson/atom-ide-purescript).

## Neovim’s built-in language server + nvim-lspconfig

As of `0.5.0`, Neovim has a built-in [language server client](https://neovim.io/doc/user/lsp.html). A popular plugin to help with configuring this server is [`nvim-lspconfig`](https://github.com/neovim/nvim-lspconfig/blob/master/README.md). This plugin includes `purescriptls` which will automatically find and root the language server as well as connect PSCIDE, etc. (for more info, read the [config](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#purescriptls)). To use, add this to your `init.lua` or inside a `EOF << lua … EOF` block in your `init.vim`.

```lua
nvim_lsp.purescriptls.setup {
  " Your personal on_attach function referenced before to include
  " keymaps & other ls options
  on_attach = on_attach,
  settings = {
    purescript = {
      addSpagoSources = true -- e.g. any purescript language-server config here
    }
  },
  flags = {
    debounce_text_changes = 150,
  }
}
```

### Vim/CoC

Configuration with [coc.nvim](https://github.com/neoclide/coc.nvim/wiki/Language-servers#purescript)

Run `:CocConfig` and add `"purescript"` in the `"languageserver"` section as follows:

```jsonc
  "languageserver": {
    "purescript": {
      "command": "purescript-language-server",
      "args": ["--stdio"],
      "filetypes": ["purescript"],
      "trace.server": "off",
      "rootPatterns": ["bower.json", "psc-package.json", "spago.dhall"],
      "settings": {
        "purescript": {
          "addSpagoSources": true,
          "addNpmPath": true // Set to true if using a local purty install for formatting
          // etc
        }
      }
    }
  }
```

CoC can be configured to format your code using the `purescript-language-server`'s formatting provider, which is backed by `purty`. If you don't have CoC-based code formatting setup in CoC already, you can add a command or key mapping like this:

```vim
command! -nargs=0  Format      :call CocAction('format')
nmap               <leader>f   :Format<cr>
```

If you want the formatter to run on save, run `:CocConfig` and add `"purescript"` to the `"coc.preferences.formatOnSaveFiletypes"`:

```jsonc
  "coc.preferences.formatOnSaveFiletypes": [
    // ...other languages
    "purescript"
  ]

```

You can also organize PureScript imports in Vim with a command and/or key mapping like this:

```vim
command! -nargs=0  OrganizeImports :call CocAction('runCommand', 'editor.action.organizeImport')
nmap               <leader>o       :OrganizeImports<cr>
```

### Vim/LanguageClient_neovim

Use [vimmer-ps](https://github.com/sriharshachilakapati/vimmer-ps).


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
