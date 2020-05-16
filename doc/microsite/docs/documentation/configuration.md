Headroom uses three different sources of configuration, where the next one eventually overrides the previous one:

1. default configuration in [embedded/default-config.yaml][file:embedded/default-config.yaml]
1. custom configuration in `.headroom.yaml`
1. command line arguments

## YAML configuration file
To check available configuration options for `.headroom.yaml`, see the [embedded/default-config.yaml][file:embedded/default-config.yaml] file, which is well documented and then override/define configuration you need in your project `.headroom.yaml`.

Below are some examples of common things you might want to configure for your project.

### Configuring License Headers
_Headroom_ contains default configuration for how _license header_ should look for selected _file type_ (e.g. to use _block comments_ `/* */` for `C++` source code files). You can check this default configuration under the `license-headers` key in [embedded/default-config.yaml][file:embedded/default-config.yaml]. For example default configuration for _Haskell file type_ looks like this:

```yaml
## Haskell configuration
haskell:
  file-extensions: ["hs"]
  margin-after: 0
  margin-before: 0
  put-after: []
  put-before: ["^module"]
  block-comment:
    starts-with: "{-|"
    ends-with: "-}"
```

If the default configuration for selected _file type_ doesn't suit your needs, you can override following settings:

1. __list of file extensions__ - if you need to match selected _file type_ with different set of file extensions, override the `file-extensions` option.
1. __margin before/after license header__ - if you want to put blank lines before and/or after license headers, use the `margin-after` or `margin-before` option, where the value is number of blank lines.
1. __put header before/after pattern__ - you can define before and/or after which pattern _Headroom_ will put the license header (such as before the line starting with `module` in _Haskell_ code).
1. __syntax of header comment__ - you can decided if your license header will be defined using multiple single-line comments (such as `//` in _C/C++_ or `--` in _Haskell_) or block comment (such as `{- -}` in Haskell). To define block comment, use the `block-comment` option with `starts-with` and `ends-with` sub-options, to use single-line comment, use `line-comment` option with `prefixed-by` sub-option.

## Command Line Arguments
Not all configuration options can be set/overridden using the command line arguments, but below is the list of matching _YAML_ options and command line options:

| YAML option         | Command Line Option            |		
|---------------------|--------------------------------|		
| `run-mode: add`     | `-a`, `--add-headers`          |		
| `run-mode: drop`    | `-d`, `--drop-headers`         |		
| `run-mode: replace` | `-r`, `--replace-headers`      |	
| `run-mode: check`   | `-c`, `--check-headers`        |		
| `source-paths`      | `-s`, `--source-path PATH`     |	
| `excluded-paths`    | `-e`, `--excluded-path REGEX`  |	
| `template-paths`    | `-t`, `--template-path PATH`   |
| `variables`         | `-v`, `--variable "KEY=VALUE"` |

Where `source-path`, `template-path` and `variable` command line arguments can be used multiple times to set more values.

## Supported License Types
_Headroom_ provides built-it license templates for major _OSS_ licenses. Whenever you need to specify the _license type_ in `.headroom.yaml` or command line arguments, use one of the values below:

| License        | Name in configuration |
|----------------|-----------------------|
| _Apache 2.0_   | `apache2`             |
| _BSD 3-Clause_ | `bsd3`                |
| _GPLv2_        | `gpl2`                |
| _GPLv3_        | `gpl3`                |
| _MIT_          | `mit`                 |
| _MPL2_         | `mpl2`                |

If you miss support for license type you use, feel free to [open new issue][meta:new-issue].

## Supported File Types
_Headroom_ can manage license headers only for supported types of source code files. Whenever you need to specify the _license type_ in `.headroom.yaml` or command line arguments, use one of the values below:

| Language     | Name in configuration | Default extensions |
|--------------|-----------------------|--------------------|
| _C_          | `c`                   | `.c`               |
| _C++_        | `cpp`                 | `.cpp`             |
| _CSS_        | `css`                 | `.css`             |
| _Haskell_    | `haskell`             | `.hs`              |
| _HTML_       | `html`                | `.html`, `.htm`    |
| _Java_       | `java`                | `.java`            |
| _JavaScript_ | `js`                  | `.js`              |
| _Rust_       | `rust`                | `.rs`              |
| _Scala_      | `scala`               | `.scala`           |
| _Shell_      | `shell`               | `.sh`              |

If you miss support for file type you use, feel free to [open new issue][meta:new-issue].


[file:embedded/default-config.yaml]: https://github.com/vaclavsvejcar/headroom/blob/master/embedded/default-config.yaml
[meta:new-issue]: https://github.com/vaclavsvejcar/headroom/issues/new

