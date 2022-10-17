# Changelog

### 0.17.0

- Diagnostics on type courtesy of @wclr.
  - Work ongoing on this area, in particular compiler changes, but I've been running with this for probably months now and it's pretty cool even now.
- Rebuilding on FFI file change (#181, @aranchelk)
  - This requires that the LSP client sends file changes for `.js` files now
- Default formatter is now none, formatter options have changed  (#180, @andys8)
- Detect new-style spago workspaces (#188, @f-f)
- Update the textDocumentSync capability to spec v3 (#182, @dariooddenino)
- Bundling changes (esmodules/build setup related, could potentially break something)

### 0.16.6

- Fix spawning formatters on windows (#160)

### 0.16.5

- Internal changes

### 0.16.4

- Over-eager suppression of X vs X(..) completion choice (#174)

### 0.16.3

-  Fix crash when adding some files due to module-header insertion 

### 0.16.2

- Insert module header into empty .purs files automatically (#169, @i-am-the-slime)
  - Currently this will use the full folder structure up to the parent non-module folder (eg `src`, `test`); in the case of 
    `test` it will also add `Test.` prefix *if* there is no top level `Test` folder

- Fix export lens positioning for kind signatures (#168)

- Suppress signature lens on inline type annotations (#166)

### 0.16.1

- Fix formatting provider issue with multi-byte characters leading to mangled results

- Fix code lenses stripping data constructor exports (#165) and add code lenses to toggle data constructor exports on/off. Tweak the text of these.

- (Internal) Bundling changes to use esbuild

- Filter code action by requested kind (#154) (possible but likely negligable minor performance improvement, possibility some language client might behave differently)

- Initial goto-defintion for local symbols

  - This provides goto-definition only (so not finding references, for example) for some locally bound identifiers based on the parsed CST

  - Navigate to e.g. arguments of top-level declarations, let bindings and arguments to let-bound functions, bindings in do blocks

  - Known issues/future thoughts:
    - Goto-defn for top level declarations is still provided by `purs ide` typings, even though we could enable navigation to some more private declarations where type info is not available

    - Let block scoping is not technically correct as the scope boundaries created by pattern bindings are not respected

    - Some binders may be missing, eg record puns 


### 0.16.0

Code lenses added:

- Add declaration type signature code lens (Idea/initial implementation @i-am-the-slime, rewritten to source data differently)

   - Enabled via config: `purescript.declarationTypeCodeLens`

- Add export management code lenses (@i-am-the-slime)
   - This features both code lenses on individual declarations, to add/remove from exports appropriately, and a module-level code lens
     to enable explicit exports if the module has implicit ones.

   - Enabled via config: `purescript.exportsCodeLense`

Other changes:

- (Internal) CST parsing shared between multiple features, currently executed on document change

- Add progress report on full build / server start

### 0.15.8

- Default `purescript.addSpagoSources` true (already true in vscode default for some time).

- Report incomplete results properly (fixes #144)

- Indicate imported module on qualifier suggestions

- Use new proposed suggestion API to give module/type info

  (at least in vscode for now)

- Provide folding ranges for declarations

- Only suggest to import constructor when type has same name (@i-am-the-slime)

- Support qualifiers in import/typo codeactions as per suggestions. Fix #143

### 0.15.7

- Show build error output when no JSON found. Fix #150

- Resolve some dependency issues affecting distribution


### 0.15.6

- Filter completion suggestions based on already imported identifiers.

  If a given identifier is imported, whether by an explicit or open import, only that same import should be suggested for that identifier.

  For example, if `length` is imported from `Data.Array`, a completion of `le` will suggest `length` from `Data.Array` but not `Data.String`; it will
   still however suggest `left` or `lengthOf`.

  Known issues:
    - Due to technical limitations data constructors are not filtered in this way
    - Depending on the `purescript.autocompleteLimit` setting, if the already imported identifier would not be in a longer list of suggestions,
    then no filtering of the other options will occur.

- Add bundled asset to published releases. This is a bundled JS file including all `npm` dependencies, requiring only `node` to run.

### 0.15.5 

- Add `purescript.fullBuildOnSave` setting which performs a full build via the configured build command instead of a IDE-server fast rebuild
  when files are saved. Disabled by default, may have bad interaction with "save all" type functionality, configuration may be subject to change
  in future.

- Introduced CST-parser for some identifier lexing, fixing issues with identifiers (specifically operators) starting with `.` in particular (#146, https://github.com/nwolverson/vscode-ide-purescript/issues/184)

- Fix #149 - autocomplete doesn't work when lines start with an "import" substring

### 0.15.4

- Auto build of opened files is now behind a setting `purescript.buildOpenedFiles` and defaulted to `false`, this should be 
  considered experimental for the time being. There are 2 issues which become more likely to be triggered by this feature,
  firstly rebuilding (even unchanged) files can cause downstream modules to require rebuilding in an incremental build ([issue](https://github.com/purescript/purescript/issues/4066)) and secondly there are reports that fast-rebuilding a file during a full/incremental build can cause corrupt output.

- Formatting provider selection: Now `purescript.formatter` can be set to `purty` (the previous formatter and still the default),
  `purs-tidy` or `pose`. Requires these tools to be already installed

- Internal changes that could avoid a case of the language server crashing abruptly

### 0.15.3

- Added `--version` CLI argument

### 0.15.2

- The code action with kind `source.organizeImports` is now the action which applies all compiler suggestions
  for unused imports, `source.sortImports` is added (previously "Organize imports") to align with the changes for JS/TS
  languages in vscode 1.57. This can be used with `editor.codeActionsOnSave` or key-bound with `editor.action.sourceAction`.
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