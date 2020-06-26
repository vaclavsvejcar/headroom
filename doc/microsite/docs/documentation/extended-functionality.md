Apart of basic functionality of adding, replacing or dropping _license headers_, which is general and same for all supported types of source code files, _Headroom_ provides some extra functionality for selected _file types_, usually in form of extracting some additional info from currently processed _source code file_ and exposing it using _dynamic variables_.

## Extended Haskell Support
This support includes extracting additional info from processed _Haskell source code file_ and exposing it using following _dynamic variables_:

- `_haskell_module_name` - Contains module name of the currently processed `Haskell` source code file. If such file contains module definition such as `module Foo where`, then this variable will contain the `Foo` value.
- `_haskell_module_copyright` - Content of the `Copyright` field of the _Haddock module header_.
- `_haskell_module_license` - Content of the `License` field of the _Haddock module header_.
- `_haskell_module_maintainer` - Content of the `Maintainer` field of the _Haddock module header_.
- `_haskell_module_stability` - Content of the `Stability` field of the _Haddock module header_.
- `_haskell_module_portability` - Content of the `Portability` field of the _Haddock module header_.
- `_haskell_module_longdesc` - Content of the long description (the one after all other fields) of the _Haddock module header_.
- `_haskell_module_shortdesc` - Content of the `Description` field of the _Haddock module header_.

### Example of Use
The use case for `_haskell_module_name` might be pretty straightforward, but you might be wondering what is the real-world use for the other variables? Well, let's say you want to use _Headroom_ to manage your _license headers_, defined as _Haddock module headers_. Initial design of _template file_ could be as following:

```mustache
{-|
Module      : {{{ _haskell_module_name }}}
Description : ??? What to put here ???
Copyright   : (c) {{ _current_year }} {{ author }}
License     : {{ license }}
Maintainer  : {{ email }}
Stability   : experimental
Portability : POSIX

??? What to put here ???
-}
```

Ok then, _Headroom_ will automatically fill the `Module`, but what to do with the `Description` and long description of the module? Well, you can use the `_haskell_module_longdesc` and `_haskell_module_shortdesc` variables to fill them automatically if you're replacing license header in file from which these values can be extracted, or you'll need to eventually fill them by hand for newly created files. You can also override their default values, that will be used if no values are extracted from processed file:

```mustache
{-|
Module      : {{{ _haskell_module_name }}}
Description : {{{ _haskell_module_shortdesc }}}
Copyright   : (c) {{ _current_year }} {{ author }}
License     : {{ license }}
Maintainer  : {{ email }}
Stability   : experimental
Portability : POSIX

{{{ _haskell_module_shortdesc }}}
-}
```

```yaml
variables:
    _haskell_module_longdesc: "!!! Long Description Here!!!"
    _haskell_module_shortdesc: "!!! Short Description Here!!!"
```

So far so good, but what about that `Copyright` field? Let's say _Headroom_ will process source code files with this _Haddock module header_:

```haskell
{-|
Module      : Foo
Description : Some short description
Copyright   : (c) 2018 First Author
              (c) 2020 Next Author
License     : BSD3
Maintainer  : some@email.com
Stability   : experimental
Portability : POSIX

Some long description
-}
```

with the above template, this `Copyright` field will be overwritten with the value of `(c) {{ year }} {{ author }}` and all other authors will be dropped. Not exactly what you'd like, right? What you can do is to use the `_haskell_module_shortdesc` variable to re-use any existing copyright statements and define the `(c) {{ _current_year }} {{ author }}` as its default value:

```mustache
{-|
Module      : {{{ _haskell_module_name }}}
Description : {{{ _haskell_module_shortdesc }}}
Copyright   : {{{ _haskell_module_copyright }}}
License     : {{ license }}
Maintainer  : {{ email }}
Stability   : experimental
Portability : POSIX

{{{ _haskell_module_shortdesc }}}
-}
```

```yaml
variables:
    _haskell_module_copyright: (c) {{ _current_year }} {{ author }}
    _haskell_module_longdesc: "!!! Long Description Here!!!"
    _haskell_module_shortdesc: "!!! Short Description Here!!!"
```

And you're done. The fact that you can use variables inside variables gives you powerful tool to define default values like this. With all the above configuration, _Headroom_ can replace old, hand-manager header like this:

```haskell
{-|
Description : Some short description
Copyright: (c) 2018 First Author
           (c) 2020 Next Author
Maintainer: some@email.com

Some long description
-}
```

Into this, without loosing any info from the old header, while respecting the structure of new header, defined by _template file_:

```haskell
{-|
Module      : Foo
Description : Some short description
Copyright   : (c) 2018 First Author
              (c) 2020 Next Author
License     : BSD3
Maintainer  : some@email.com
Stability   : experimental
Portability : POSIX

Some long description
-}
```