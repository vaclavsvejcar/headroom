Version `0.4.1.0` is minor release that brings some improvements and bugfixes, without any breaking changes.

## New & Noteworthy

### Go language support
This release adds support for Go. Basic support includes management of license headers [(adding, replacing, removing)][doc:running-headroom]. Default configuration expects that _Go_ source code files have `.go` file extension and that templates are named `go.mustache`. Also the license header is expected to use single line comments (`//`). Check [Configuration][doc:configuration] chapter for more details.


## Other bugfixes and improvements

[doc:configuration]: documentation/configuration.md
[doc:migration-guide]: migration-guide.md
[doc:running-headroom]: documentation/running-headroom.md
[github/issue/57]: https://github.com/vaclavsvejcar/headroom/issues/57
[github/issue/60]: https://github.com/vaclavsvejcar/headroom/issues/60
[github/issue/62]: https://github.com/vaclavsvejcar/headroom/issues/62