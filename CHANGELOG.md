# Changelog


### 0.8.0

- Add suggestion ranking heuristics, currently these are for qualified import suggestiosn https://github.com/nwolverson/purescript-language-server/pull/15 @natefaubion

- Add configurable Prelude open import via `preludeModule` https://github.com/nwolverson/purescript-language-server/pull/16 @natefaubion

- Use Markdown for suggestion details https://github.com/nwolverson/purescript-language-server/pull/23 @Krzysztof-Cieslak

### 0.7.1 

- Automatically add qualified imports on completion with an unknown qualifier https://github.com/nwolverson/purescript-language-server/pull/7 @natefaubion

- Fix extraneous newlines in case split/add clause https://github.com/nwolverson/purescript-language-server/pull/9

### 0.6.0

- Add changelog :)

- Support psc-package source globs. Toggled via `addPscPackageSources` config (default `false`) and using `psc-package sources` command.

- Show expanded type in tooltips (when different from non-expanded one)

- Show module tooltip for qualified imports (hover over the qualifier of a qualified identifier)