Version `0.4.1.0` is minor release that brings some improvements and bugfixes, without any breaking changes.

## New & Noteworthy

### Go language support
This release adds support for Go. Basic support includes management of license headers [(adding, replacing, removing)][doc:running-headroom]. Default configuration expects that _Go_ source code files have `.go` file extension and that templates are named `go.mustache`. Also the license header is expected to use single line comments (`//`). Check [Configuration][doc:configuration] chapter for more details.

### Improved console output
In previous versions _Headroom_ always printed out full list of processed files, whether changed or not, so it was sometimes tedious to find changes in full logs for large project. In current version, only files requiring attention (either changed or needed to be changed by user) are printed out.

[![asciicast](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8.svg)](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8)


## Other bugfixes and improvements
- [#72][github/issue/72] Headroom doctest not passing with `hashable-1.3.1.0`


[doc:configuration]: documentation/configuration.md
[doc:migration-guide]: migration-guide.md
[doc:running-headroom]: documentation/running-headroom.md
[github/issue/72]: https://github.com/vaclavsvejcar/headroom/issues/72