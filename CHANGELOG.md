# Changelog
All notable changes to this project will be documented in this file.

## 0.3.2.0 (not released yet, TBD)
- Bump _LTS Haskell_ to `16.20`
- Minor improvements & bugfixes

## 0.3.1.0 (released 2020-08-18)
- [#48] Decouple IO actions for easier testing
- [#53] Add support for _PureScript_ source code files
- [#54] Extract _dynamic variables_ from _Java_ source code files
- Bump _LTS Haskell_ to `16.10`

## 0.3.0.0 (released 2020-06-28)
- [#25] Extract _dynamic variables_ from Haskell source code files
- [#29] Add _dynamic variable_ for current year and add support for updating years
- [#30] Allow workflow without configuration and template files
- [#49] Microsite for documentation ([https://doc.norcane.com/headroom/latest/](https://doc.norcane.com/headroom/latest/))
- [#51] Allow template variable values to be templates itself
- [#52] Post-processing functions for _license headers_
- Various bugfixes and performance improvements
- Bump _LTS Haskell_ to `16.2`

## 0.2.2.1 (released 2020-05-08)
- Minor improves in generated `.headroom.yaml` in `init` mode

## 0.2.2.0 (released 2020-05-04)
- [#45] Add `-c|--check-headers` command line option
- Bump _LTS Haskell_ to `15.11`

## 0.2.1.1 (released 2020-04-30)
- [#47] Make possible to build Headroom with GHC 8.10
- Remove unused dependency on `text` package.

## 0.2.1.0 (released 2020-04-29)
- [#41] Add `--dry-run` option to allow test run without changing files.
- [#44] Don't touch files whose contents have not changed.
- [#46] Add `-e|--excluded-path=REGEX` option to exclude source paths.
- Bump _LTS Haskell_ to `15.10`.
- Remove unused dependencies.

## 0.2.0.0 (released 2020-04-25)
- [#28] Allow license headers to be anywhere in the file, not only at the very beginning.
- [#31] Render templates for each source file instead of once (blocker for [#25])
- [#32] Allow custom user configuration for license headers.
- [#34] Support for [Rust](https://www.rust-lang.org/)
- [#35] Support for [Bash](https://www.gnu.org/software/bash/)
- [#36] Support for _C/C++_
- [#38] Add `-a|--add-headers` command-line option
- bump _LTS Haskell_ to `15.9`

## 0.1.3.0 (released 2020-03-23)
- [#24] Added _Init_ command that automatically creates initial _Headroom_ configuration and set of templates.
- bump _LTS Haskell_ to `15.5`

## 0.1.2.0 (released 2020-03-06)
- FIXED: missing `test-data/` folder in dist tarball prevents tests execution
- bump _LTS Haskell_ to `15.2`

## 0.1.1.0 (released 2020-02-26)
- FIXED: missing `embedded/` folder in dist tarball for _Hackage_ prevents successful build using _Cabal_
- bump _LTS Haskell_ to `15.1`

## 0.1.0.0 (released 2020-02-20)
- initial release
