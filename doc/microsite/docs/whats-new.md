## New & Noteworthy

### Support for Dynamic Template Variables
Until now, _Headroom_ allowed to use _static variables_ only. _Static variables_ are defined by used either in _YAML configuration_ or as _command line argument_ and their values are same for all processed _source code files_.

On the other hand, _dynamic variables_ are produced by _Headroom_ during runtime and can be either the same for all processed _source code files_ or may differ file from file. _Dynamic variables_ also stars with _underscode_ symbol, so you can easy tell which one is dynamic and which static. Following _global dynamic variables_ were added as part of this update:

- `_current_year` - When used in template file, this variable will be always replaced by the value of current year (e.g. `2020`).

### Template Variables are Templates as well
From now, each _template variable_ defined in `variables` part of _YAML configuration_ is also considered as template, so you can use variables inside variables, as long as you

1. don't call variables recursively
1. don't address variables in cyclic dependency

Example of how you can use this is to compose `copyright` variable from existing `author` _static variable_ and `_current_year` dynamic variable:

```yaml
variables:
    author: John Smith
    copyright: Copyright (c) {{ _current_year }} {{ author }}
```

### Extracting Dynamic Variables from Existing Haddock Module Headers
Let's say you want to use _Headroom_ to manage your _license headers_ that are also _Haddock module headers_, but you want to reuse values of some fields from already existing _Haddock module headers_ (e.g. `Copyright`, `Maintainer`, etc). From this release, when _Headroom_ processes _Haskell source code files_, it will automatically extract value of possibly existing _Haddock module header_ and expose them through _dynamic variables_. You can then define your _template file_ as following:

```mustache
{-|
Module      : {{{ _haskell_module_name }}}
Description : {{{ _haskell_module_shortdesc }}}
Copyright   : {{{ _haskell_module_copyright }}}
License     : {{ license }}
Maintainer  : {{ email }}
Stability   : experimental
Portability : POSIX

{{{ _haskell_module_longdesc }}}
-}
```

If the processed source file will contain _Haddock module header_ with the `Copyright` field, such field value will be exposed as `_haskell_module_copyright`. You can also define default values in case that you process new files or files without _Haddock module header_, as following:

```yaml
variables:
    author: John Smith
    _haskell_module_copyright: Copyright (c) {{ _current_year }} {{ author }}
```

And yes, you can use variables inside variables, isn't that great? :-) See [Extended Functionality][rel:extended-functionality] chapter for more details.


### Support for Updating Years in Copyright Statements
This version introduces new concept called _post-processing functions_. These functions allow performing some extra post-processing on rendered _license headers_. For example, you can use the `update-copyright` post-processing function to keep years in your copyright statemets up-to-date. see [Post-processing Functions][rel:post-processing] chapter for more details.

### Support Workflow without Template and Configuration Files
If you plan to use _Headroom_ occasionally or don't like the idea to generate configuration/template file into your project, you can use basic functions of _Headroom_ using only _command line options_. See [Configuration][rel:configuration] chapter for more details.

## Migrating from v0.2.1.0
There are no breaking changes in _CLI API_ or _YAML configuration_ from last public release `v0.2.1.0`, so upgrading to `v0.3.0.0` should be drop-in replacement.

[rel:extended-functionality]: documentation/extended-functionality.md
[rel:configuration]: documentation/configuration.md
[rel:post-processing]: documentation/post-processing.md