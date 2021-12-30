Version `0.4.3.0` is minor release that brings some improvements and bugfixes, without any breaking changes in _CLI_ interface or configuration.

## New & Noteworthy

### Support for Dart language
This version adds basic support for managing license/copyright headers in _Dart_ source code files. Default configuration assumes that licence/copyright header is the first single-line comment (or more, but not separated by _newline_), such as:

```dart
// Super Awesome App
// Copyright (c) 2021 John Smith
// this is still copyright header

// this is not copyright header anymore

void main() {
  /* another comment */
  print('Hello, World!');
}
```

### Support for PHP language
This version adds support for managing license/copyright headers in _PHP_ source code files. Default configuration assumes that license/copyright header is the very first _DocBlock_ comment present in _PHP_ file, such as:

```php
<?php

/**
 * This is a file-level DocBlock, considered as license/copyright header
 * 
 * A warning will be raised, saying that to document the define, use
 * another DocBlock
 * @package SomePackage
 */

define('foo', 'bar');

```

At this moment, only basic support is present (i.e. no _dynamic variables_ are extracted from the header), but this might change in future releases.

### Global Configuration
Since this release, apart from usual per-project configuration that _Headroom_ uses (`.headroom.yaml` files), it also adds _global configuration_, stored in user's home directory (`~/.headroom`), where some global config values can be changed, such as how often (and if) _Headroom_ should check for updates. See [Configuration Overview][doc:configuration] chapter for more details.

### Automatically check for updates
This release adds functionality that automatically checks whether new version of _Headroom_ is available and if yes, it notifies user in command line. By default, this happens once a week and it can be changed (or disabled) in _global configuration_. See [Configuration Overview][doc:configuration] chapter for more details.

!!! note
    Info about new version is retrieved using _GitHub REST API_ from project releases page, no personal info is collected and no tracking is done. Your privacy is priority for us and always will be respected.

## Other bugfixes and improvements
- [[#83]][github/issue/83] - _Update Copyright_ post-processor incorrectly updates years in some cases
- minor tweaks and performance improvements

[doc:configuration]: documentation/configuration.md
[doc:migration-guide]: migration-guide.md
[doc:running-headroom]: documentation/running-headroom.md
[github:vcs-ignore]: https://github.com/vaclavsvejcar/vcs-ignore
[github/issue/83]: https://github.com/vaclavsvejcar/headroom/issues/83