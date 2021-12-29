<p align="center"><img src ="https://github.com/vaclavsvejcar/headroom/blob/master/doc/assets/logo.png?raw=true" width="200" /></p>

![CI](https://github.com/vaclavsvejcar/headroom/workflows/CI/badge.svg)
[![Hackage version](http://img.shields.io/hackage/v/headroom.svg)](https://hackage.haskell.org/package/headroom)
[![Stackage version](https://www.stackage.org/package/headroom/badge/lts?label=Stackage)](https://www.stackage.org/package/headroom)

Would you like to have nice, up-to-date license/copyright headers in your source code files but hate to manage them manually? Then __Headroom__ is the right tool for you. Define your license header as [Mustache][web:mustache] template, put any template variables into [YAML][wiki:yaml] configuarion file, and you're ready to go!

__Headroom__ also offers much more than just [adding, replacing or dropping][doc:running-headroom] license headers. It can also [update years in copyrights][doc:post-processing] for you, provides [content-aware templates][doc:templates] for some source code file types, and much more!

[![asciicast](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8.svg)](https://asciinema.org/a/DkSBMZPHMJvJ4jyDtvT9ehfs8)

## Main Features
- __License Header Management__ - [Add, replace or drop license headers][doc:running-headroom] in your source code files with a single command. What's more, contrary to many similar tools, _Headroom_ allows you to also replace/drop headers that weren't previously generated by _Headroom_, using smart header auto-detection.
- __Powerful Customization__ - Default [configuration][doc:configuration] should cover most use-cases, but if you need to put empty lines before/after generated header, use different comment style of headers, you can customize the configuration to match exactly your needs.
- __Built-in OSS License Headers__ - If you want to use license header for one of the popular OSS licenses, then __Headroom__ can [generate them for you][doc:running-headroom#gen-command] - no need to search for them on web.
- __Automatic Initialization for OSS Projects__ - Setting up external tools like _Headroom_ for your project can be boring. Fortunately, _Headroom_ can [initialize itself][doc:running-headroom#init-command] for your project, by generating configuration file and template files.
- __Content-aware Templates__ - for selected file types, _Headroom_ [exposes additional info][doc:templates] about processed file using template variables, so you can use info like _Java_ package name in your templates.
- __Copyright Year Updater__ - _Headroom_ is good not only for basic license header management, but also for further processing of generated headers. Do you need to [update years in your copyrights][doc:post-processing]? No problem!

## Installation
You can get _Headroom_ via one of the following options:
1. download pre-built binary for _GNU/Linux_ or _macOS_ (x64) from [releases page][meta:releases]
1. install using [Homebrew][web:homebrew]: `brew install norcane/tools/headroom`
1. build from source code - see [project microsite][web:headroom] for more details

## Adopters
Here is the list of projects using _Headroom_. If you're using _Headroom_ and aren't on the list, feel free to [submit new issue][meta:new-issue] or [pull request][meta:pulls].

- [kowainik/hit-on](https://github.com/kowainik/hit-on) - Kowainik Git Workflow Helper Tool
- [kowainik/summoner](https://github.com/kowainik/summoner) - Tool for scaffolding batteries-included production-level Haskell projects
- [wireapp/wire-server](https://github.com/wireapp/wire-server) - Wire back-end services (https://wire.com)

## Mentions
- [Bind The Gap magazine, issue #2](https://bindthegap.news/issues/02dec2020.html) has chapter dedicated to _Headroom_ (pages _17-18_).

## Documentation
* for end-user documentation, [official project microsite][web:headroom]
* for _Haskell API_ documentation, see [Headroom on Hackage][hackage:headroom]

### Running microsite locally
If you need to show microsite documetation for unreleased version of _Headroom_ (e.g. for `master` branch), you can run it locally using [MkDocs][web:mkdocs] tool:

```
$ cd doc/microsite/
$ mkdcs serve
```

Documentation is then available on http://127.0.0.1:8000.

[i25]: https://github.com/vaclavsvejcar/headroom/issues/25
[hackage:headroom]: https://hackage.haskell.org/package/headroom
[meta:new-issue]: https://github.com/vaclavsvejcar/headroom/issues/new
[meta:pulls]: https://github.com/vaclavsvejcar/headroom/pulls
[meta:releases]: https://github.com/vaclavsvejcar/headroom/releases
[web:headroom]: https://doc.norcane.com/headroom/latest/
[web:homebrew]: https://brew.sh
[doc:configuration]: https://doc.norcane.com/headroom/latest/documentation/configuration/
[doc:templates]: https://doc.norcane.com/headroom/latest/documentation/templates/
[doc:post-processing]: https://doc.norcane.com/headroom/latest/documentation/post-processing/
[doc:running-headroom]: https://doc.norcane.com/headroom/latest/documentation/running-headroom/
[doc:running-headroom#gen-command]: https://doc.norcane.com/headroom/latest/documentation/running-headroom/#gen-command
[doc:running-headroom#init-command]: https://doc.norcane.com/headroom/latest/documentation/running-headroom/#init-command
[web:mkdocs]: https://www.mkdocs.org
[web:mustache]: https://mustache.github.io
[wiki:yaml]: https://en.wikipedia.org/wiki/YAML
