Version `0.4.0.0` is major release of _Headroom_ and introduces both new cool features, but also some breaking changes in cofiguration format. Please pay attention to below text and don't forget to check [Migration Guide][doc:migration-guide] if you're migrating from version `0.3.x.y`.

## New & Noteworthy

### Extended margin options for license headers
In previous version of _Headroom_, if margin (empty lines) around generated header is needed, one can use the `margin-before` and `margin-after` config properties. These allow to define margin between header and preceding/following code, but aren't applied when header is very first (or very last) thing in the source code. This can cause trouble for example when keeping blank line after header, which is last thing in the file, is required.

In order to make this more flexible, current two configuration options are dropped and replaced by following ones:

- `margin-top-code` - defines margin (in no. of empty lines) between generated header and code above it, but only **IF** header is **NOT** the first thing in file.
- `margin-top-file` - defines margin (in no. of empty lines) between generated header the very top of the file, but only **IF** header **IS** the first thing in file.
- `margin-bottom-code` - defines margin (in no. of empty lines) between generated header and code below it, but only **IF** header is **NOT** the last thing in file.
- `margin-bottom-file` - defines margin (in no. of empty lines) between generated header the very end of the file, but only **IF** header **IS** the last thing in file.

#### Examples

```haskell
                               -- <<< 'margin-top-file' set to '1'
{- some header -}
                               -- <<< 'margin-bottom-code' set to '2'

someFunc :: String
someFunc = undefined
```

### Version command line option
Two new command line options were added to check version of current _Headroom_ installation, where `--version` returns version and some additional info and `--numeric-version` returns only version number (for example for some programmatic checks):

```
$ headroom --version
headroom, v0.4.0.0 :: https://github.com/vaclavsvejcar/headroom

$ headroom --numeric-version
0.4.0.0
```

### Regular expressions used to specify header start/end
_Headroom_ now provides more flexibility to define copyright/license header start/end by using regular expressions in `starts-with`/`ends-with` (under `block-comment` key) and `prefixed-by` (under `line-comment`) values. This is a breaking change, so don't forget to check [Migration Guide][doc:migration-guide] for more details if you're using these fields in configuration.


### Compatibility check for `.headroom.yaml`
_Headroom's_ configuration file `.headroom.yaml` now requires `version` field, which helps _Headroom_ to check whether your configuration is compatible with its current version and if not, _Headroom_ is able to guide you through correct migration steps based on this value. You usually only need to bump this version when breaking changes in configuration are introduced and changes in your configuration files are required. You will be always informed about such need. See [Migration Guide][doc:migration-guide] for more details.

## Other bugfixes and improvements
- [#57][github/issue/57] fixes the issue where _Headroom_ sometimes incorrectly detect copyright/license header position within other comment in source code.
- [#60][github/issue/60] tries to make error messages more helpful for end user by providing links to this documentation microsite
- [#62][github/issue/62] makes sure that produced header should have correct comment syntax (thus not breaking surrounding source code)


[doc:migration-guide]: migration-guide.md
[github/issue/57]: https://github.com/vaclavsvejcar/headroom/issues/57
[github/issue/60]: https://github.com/vaclavsvejcar/headroom/issues/60
[github/issue/62]: https://github.com/vaclavsvejcar/headroom/issues/62