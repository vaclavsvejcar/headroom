Version `0.4.2.0` is minor release that brings some improvements and bugfixes, without any breaking changes in _CLI_ interface or configuration.

## New & Noteworthy

### Support for URL-based templates
Until now, _Headroom_ allowed to use only either _built-in_ templates or templates loaded from local file system. This version adds option to load templates from URL (at this moment only _HTTP_ and _HTTPS_ protocols are allowed). To use URL templates, just put the _HTTP(S)_ address where you normally put path to local template, i.e.:

```
-t|--template-file https://example.com/path/haskell.mustache
```

For command line or

```yaml
template-paths:
  - /some/local/haskell.mustache
  - https://example.com/path/rust.mustache
```

for _YAML_ configuration.

Note that you can freely combine all template types (i.e. _built-in templates_, _local templates_ and _URL templates_), but in case that same template will be define from multiple sources, following order will be applied to select one (left to right from least to most preferred):

```
built-in template > URL template > local template
```

Also note that naming convention is the same as for local templates, i.e.

```
http(s)://path/to/<FILE_TYPE>.<TEMPLATE_TYPE>
```

See [configuration chapter][doc:configuration] for more details.


### Automatically exclude source paths ignored by GIT
It may happen that some (otherwise valid) source files inside your source directory are for some reason set to be ignored by _Git_, therefore you also need _Headroom_ to ignore them. Until now, this has to be done manually using the `exclude-source-paths`/`--exclude-source-path=PATH` _YAML_/_CLI_ options. Thanks to the [vcs-ignore][github:vcs-ignore] library, _Headroom_ can not exclude these files for you by scanning _Git's_ ignore rules. All you need to do is use either `exclude-ignored-paths: true` _YAML_ option or `--exclude-ignored-paths` _CLI_ option.

See [configuration chapter][doc:configuration] for more details.

## Other bugfixes and improvements
TODO

[doc:configuration]: documentation/configuration.md
[doc:migration-guide]: migration-guide.md
[doc:running-headroom]: documentation/running-headroom.md
[github:vcs-ignore]: https://github.com/vaclavsvejcar/vcs-ignore
[github/issue/72]: https://github.com/vaclavsvejcar/headroom/issues/72