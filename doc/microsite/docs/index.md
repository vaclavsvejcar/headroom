<p align="center"><img src ="images/logo.png" width="200" /></p>

Would you like to have nice, up-to-date license/copyright headers in your source code files but hate to manage them by hand? Then __Headroom__ is the right tool for you! Now you can define your license header as [Mustache][web:mustache] template, put all the variables (such as author's name, year, etc.) into the [YAML][wiki:yaml] config file and Headroom will take care to add such license headers to all your source code files.

[![asciicast](https://asciinema.org/a/4Pfxdss0V4msFjjt2z6mgCZCp.svg)](https://asciinema.org/a/4Pfxdss0V4msFjjt2z6mgCZCp)

# Main Features
- __License Header Management__ - allows to add, replace or drop license headers in source code files.
- __Flexible Header Detection__ - you can even replace or drop license headers that weren't generated by Headroom, as they are automatically detected from source code files, not from template files.
- __Fully Customizable__ - would you like to put empty lines before/after header? Or use different style of comments for your headers? No problem, you can change this in configuration.
Headroom, as they are automatically detected from source code files, not from template files.
- __Template Generator__ - generates license header templates for most popular _open source_ licenses. You can use these as-is, customize them or ignore them and use your custom templates.
- __Automatic Initialization__ - using the _Init_ command, _Headroom_ can detect what source code files you have in your project and generate initial configuration file and appropriate template skeletons.

# Adopters
Here is the list of projects using _Headroom_. If you're using _Headroom_ and aren't on the list, feel free to [submit new issue][meta:new-issue] or [pull request][meta:pulls].

- [wire-server](https://github.com/wireapp/wire-server) - Wire back-end services [https://wire.com](https://wire.com)


[meta:new-issue]: https://github.com/vaclavsvejcar/headroom/issues/new
[meta:pulls]: https://github.com/vaclavsvejcar/headroom/pulls
[web:mustache]: https://mustache.github.io
[wiki:yaml]: https://en.wikipedia.org/wiki/YAML