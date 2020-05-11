Before we'll dive deeper into describing features and configuration options, let's introduce some _top level_ overview of how _Headroom_ works and what it does.

## Why Headroom?
It's good to have copyright/license headers at the very top of each source code file, but it's not only cumbersome but also prone to errors to manage them by hand. You add some new source files in rush and forgot to copy the license header into them. Or you just need to update some info in them and hope that _find&replace_ will do the job, and so on. Why to do this by hand, when some software could do that for you? This is why was _Headroom_ created. The only thing you need to do is to set up _Headroom_ for your project and then it will do all the dirty job for you.

## How it works?
Simply said, _Headroom_ takes [Mustache][web:mustache] templates of license headers, fills any variables by values taken from `.headroom.yaml` configuration file, renders them and puts them onto specified position in your source code files. And that's pretty much all. It has some advanced configuration options and another fancy features, but we'll get back to them in later chapters.

### Configuration
Main source of configuration is the `.headroom.yaml` file that you put into your project directory. This is the place where _Headroom_ looks for things such as where are source code files stored, where are license template files and what variables should be replaced in templates. It also offers some more advanced options such as to specify before/after which pattern to put the license headers, define blank lines as margin around the license header, etc. See more details in [Configuration][rel:Configuration] chapter.

### Source Code Files
Those are the files in which the license headers should be managed by _Headroom_. You can specify multiple different locations and also paths that should be excluded. See [Configuration][rel:Configuration] chapter for more details.

### License Templates
Those are the templates from which the final license headers are generated. _Headroom_ uses [Mustache][web:mustache] as lightweight and easy templating system. Values for variables used in templates are then defined either in the `.headroom.yaml` file or using command line arguments. See [Configuration][rel:Configuration] chapter for more details.

### Variables
todo


[rel:Configuration]: configuration.md
[web:mustache]: https://mustache.github.io/