Headroom uses three different sources of configuration, where the next one eventually overrides the previous one:

1. default configuration in [embedded/default-config.yaml][file:embedded/default-config.yaml]
1. custom configuration in `.headroom.yaml`
1. command line arguments

## YAML configuration file
To check available configuration options for `.headroom.yaml`, see the [embedded/default-config.yaml][file:embedded/default-config.yaml] file, which is well documented and then override/define configuration you need in your project `.headroom.yaml`.

## Command Line Arguments
Not all configuration options can be set/overridden using the command line arguments, but below is the list of matching _YAML_ options and command line options:

| YAML option         | Command Line Option            |		
|---------------------|--------------------------------|		
| `run-mode: add`     | `-a`, `--add-headers`          |		
| `run-mode: drop`    | `-d`, `--drop-headers`         |		
| `run-mode: replace` | `-r`, `--replace-headers`      |	
| `run-mode: check`   | `-c`, `--check-headers`        |		
| `source-paths`      | `-s`, `--source-path=PATH`     |	
| `excluded-paths`    | `-e`, `--excluded-path=REGEX`  |	
| `template-paths`    | `-t`, `--template-path=PATH`   |
| `variables`         | `-v`, `--variable="KEY=VALUE"` |

Where `source-path`, `template-path` and `variable` command line arguments can be used multiple times to set more values.

## Configuration Tips
This chapter contains tips on the most common configuration changes you may want to use in your project.

### Adding blank lines before/after license header
If you want to configure Headroom to put blank lines before or after (or both) the license header, you can use following _YAML_ configuration:

```yaml
license-headers:
  <FILE_TYPE>:
    margin-after: 1   # number of blank lines to put after license header
    margin-before: 1  # number of blank lines to put before license header
```

### Putting license header before/after selected pattern
If you need to put the license header before, after (or both) selected patterns, e.g. before the `package foo.bar` line in _Java_ files or after the _language pragmas_ in _Haskell_ files, you can use the `put-before` and/or `put-after` configuration keys.

#### put-before option
`put-before` accepts list of regular expressions and the license header is placed before the very first line matching one of the given expressions.

__Example configuration:__
```yaml
license-headers:
  c:
    put-before: ["^#include"]
```

__Result:__
```c
/* >>> header is placed here <<< */
#include <stdio.h>
#include <foo.h>
int main() { ... }
```

#### put-after option
`put-after` accepts list of regular expressions and the license header is placed after the very last line matching one of the given expressions.

__Example configuration:__
```yaml
license-headers:
  c:
    put-after: ["^#include"]
```

__Result:__
```c
#include <stdio.h>
#include <foo.h>
/* >>> header is placed here <<< */
int main() { ... }
```


[file:embedded/default-config.yaml]: https://github.com/vaclavsvejcar/headroom/blob/master/embedded/default-config.yaml
