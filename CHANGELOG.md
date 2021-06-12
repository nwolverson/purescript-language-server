# Changelog

- Show a warning dialog (with build option) on start if we get an externs out of date error
- Remove deprecated editor mode/polling purs ide config (Removed in 0.13.8)

### 0.15.1

- Add `flake.nix` and `shell.nix` to the list of files that indicate a PS project may be present #136, #137 (@ursi)
- Change the way the `purty` formatter is spawned to make it faster
- Don't fix implicit prelude in all (import) suggestions. #108
- Add auto build of opened files #125 (@wclr)
- Build with PureScript 0.14.x, CI udpates

### 0.15.0

- Add support for importing conflicting identifiers #118 (@i-am-the-slime)
- Parse build output from both stdout/stderr (required for PureScript 0.14.0). #111
- Prioritize "Organise Imports" action lower than others #113

### 0.14.4 

- Ident namespaces - multiple completions requested, to provide correct imports for importing same-named identifiers in different namespaces
- Organise imports code action - provides a code action to trigger the command purescript.organiseImports, this is of kind source.organizeImports

### 0.14.3

- Add command organiseImports - takes 1 uri param, organises imports …

### 0.14.2
### 0.14.1

- Temporarily disable broken import namespace filter

### 0.14.0

- Formatting provider using purty
- Fix some issues in goto definition
- Fix some issues in find references
- Change typo code action no longer requires custom LSP client support, instead gives multiple code actions
- Fix code action range
- Fix build command parsing

### 0.13.7

- Fix case split/add clause

### 0.13.6

- Server start improvements
- Fix code actions not working for hole/typo due to range inversion
- Update obsolete settings 

### 0.13.5

- Published artifacts fix

### 0.13.4

- Filter overlapping edits (better apply-all suggestions)
- Deduplicate replacments


### 0.13.3 

- CWD issues on server start - generally relevant to unusual situations or other LSP clients

### 0.13.2

- Fixing broken release

### 0.13.1 

- check if PURS_IDE_SOURCES env var is being used in startup

### 0.13.0

- allow setting package source globs using PURS_IDE_SOURCES
- allow Prim modules to be completed
- new LS pull-based configuration API
- Complete existing module qualifiers 
- Startup logging improvements

### 0.12.8

- Add fold provider for imports

### 0.12.7

Spago support:

- Propagate the entire environment when running the build command. #45
- Recognise spago.dhall as identifying a Purescript project.
- Add new config option for calling `spago sources` to get sources globs

### 0.12.6

- Fix relative path issue in diagnostics

### 0.12.5 

- Improve initial load failure case (slow startup)

### 0.12.4

- Correct use of root path/ root uri
- Update arg parsing, add error log option

### 0.12.3 

- Build error improvements

### 0.12.2

- Allow codegen targets to be specified

### 0.12.1

- Fix incorrect range check suppressing all specific error code actions

### 0.12.0

- PureScript 0.12.0 compat

### 0.11.0

- Add find references command (requires purs 0.12). Currently works at the value level

### 0.10.2

- Add warning/build option on missing output directory - https://github.com/nwolverson/purescript-language-server/commit/83e7f2b884915100318bb6a06eb5b59fd7e39354

### 0.10.1 

- Respect `pscIdePort` config - when absent port will be auto-chosen, when present server will be found or started on that port
- Respect `autoStartPscIde` config
- Make `executeCommandProvider` optional
- `fixTypo` position fix

### 0.10.0

- Replace typed hole command & code action (requires LSP client support) https://github.com/nwolverson/purescript-language-server/issues/14
- Move dependencies from purescript-ide-purescript-core
- Configurable output directory #30
- Fix all suggestions commands https://github.com/nwolverson/purescript-language-server/issues/12

### 0.9.0

- Add documentation to hover tooltips https://github.com/nwolverson/purescript-language-server/pull/25 [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)
- Make compiler fixes (particularly import fixes) not leave extra blank lines https://github.com/nwolverson/purescript-language-server/issues/13
- Fix `preludeModule` adding a prelude import if it is already imported explicitly https://github.com/nwolverson/purescript-language-server/issues/26
- Ensure IDE server dependencies are reloaded on full build (particularly in case of editor mode) https://github.com/nwolverson/purescript-language-server/issues/19
- Fix completion edits in some circumstances https://github.com/nwolverson/vscode-ide-purescript/issues/96

### 0.8.0

- Add suggestion ranking heuristics, currently these are for qualified import suggestions https://github.com/nwolverson/purescript-language-server/pull/15 [@natefaubion](https://github.com/natefaubion)
- Add configurable Prelude open import via `preludeModule` https://github.com/nwolverson/purescript-language-server/pull/16 [@natefaubion](https://github.com/natefaubion)
- Use Markdown for suggestion details https://github.com/nwolverson/purescript-language-server/pull/23 [@Krzysztof-Cieslak](https://github.com/Krzysztof-Cieslak)

### 0.7.1 

- Automatically add qualified imports on completion with an unknown qualifier https://github.com/nwolverson/purescript-language-server/pull/7 [@natefaubion](https://github.com/natefaubion)
- Fix extraneous newlines in case split/add clause https://github.com/nwolverson/purescript-language-server/pull/9

### 0.6.0

- Add changelog :)
- Support psc-package source globs. Toggled via `addPscPackageSources` config (default `false`) and using `psc-package sources` command.
- Show expanded type in tooltips (when different from non-expanded one)
- Show module tooltip for qualified imports (hover over the qualifier of a qualified identifier)