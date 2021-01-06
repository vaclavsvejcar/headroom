To allow users to define how copyright headers should look like, Headroom uses [Mustache][web:mustache] templates.

## How it works
Every template file holds template of copyright header for selected source code file type, where name of the template file must be `<FILE_TYPE>.mustache`. Headroom then needs to be configured in order to know, in which folder(s) it should look for template files (see [Configuration page][rel:configuration] for more details).

## Template Variables
Main point of copyright headers to be defined as template is to allow end users to use template variables. Values for these variables are either configured in `.headroom.yaml`, or passed via command line arguments (see [Configuration page][rel:configuration] for more details), or are provided by Headroom itself.

### Static Variables
_Static variables_ are variables whose values remains same during single Headroom run and are same for all processed source code file. Values for these variables are either configured in `.headroom.yaml`, or passed via command line arguments (see [Configuration page][rel:configuration] for more details). These variables are usually like author's name, project name, etc.

### Dynamic Variables
_Dynamic variables_, on the other side, are not provided by end user, but by Headroom, and may be either same for all processed source code files, or specific to individual ones. Dynamic variable names starts by underscode (`_`) by convention, to distinguish them from static ones.

In current version of Headroom, below is the list of dynamic variables, shared for all processed source code file:

- `_current_year` - value of the current year (e.g. `2020`)

Very useful are dynamic variables that are specific to currently processed source code file, as they can expose additional info that can be used in templates (e.g. package name of _Java_ file, etc.). This concept is called _content-aware templates_ in Headroom.

## Content-aware Templates
For selected file types Headroom extracts additional information from currently processed source code file and exposes them as _dynamic variables_, so resulting rendered template can contain details about the concrete file. This allows users to use info such as package name of _Java_ source code, _Haddock_ field values for _Haskell_ files, etc. Below is the list of file types that currently supports this.

### Haskell Support
This support includes extracting additional info from processed _Haskell source code file_ and exposing it using following _dynamic variables_:

- `_haskell_module_name` - Contains module name of the currently processed `Haskell` source code file. If such file contains module definition such as `module Foo where`, then this variable will contain the `Foo` value.
- `_haskell_module_copyright` - Content of the `Copyright` field of the _Haddock module header_.
- `_haskell_module_license` - Content of the `License` field of the _Haddock module header_.
- `_haskell_module_maintainer` - Content of the `Maintainer` field of the _Haddock module header_.
- `_haskell_module_stability` - Content of the `Stability` field of the _Haddock module header_.
- `_haskell_module_portability` - Content of the `Portability` field of the _Haddock module header_.
- `_haskell_module_longdesc` - Content of the long description (the one after all other fields) of the _Haddock module header_.
- `_haskell_module_shortdesc` - Content of the `Description` field of the _Haddock module header_.

#### Example of Use
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

### Java Support
This support includes extracting additional info from processed _Java source code file_ and exposing it using following _dynamic variables_:

- `_java_package_name` - Contains package name of the currently processed `Java` source code file.

### PureScript Support
This support includes extracting additional info from processed _PureScript source code file_ and exposing it using following _dynamic variables_:

- `_purescript_module_name` - Contains module name of the currently processed `PureScript` source code file. If such file contains module definition such as `module Foo where`, then this variable will contain the `Foo` value.

[web:mustache]: https://mustache.github.io
[rel:configuration]: configuration.md

