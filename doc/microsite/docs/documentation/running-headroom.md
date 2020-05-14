After you set up _Headroom_ for your project, you should be ready to run it. Below is the overview of the _command line interface_ that _Headroom provides_.

## Commands
_Headroom_ provides three different commands, you might already familiar with the `gen` or `init` command from the [Project Setup Guide][rel:project-setup-guide] chapter:

1. __run__ - this is the main command and the one you'll use the most. It allows you to add, replace, drop or check license headers in source code files.
1. __gen__ - this command allows to generate files stub required by _Headroom_, such as _YAML_ configuration file or license templates for selected _OOS_ license.
1. __init__ - this command simplifies _Headroom_ setup by automatically scanning your project and generating proper `.headroom.yaml` and template files. 

### Run Command
Let's start with the most useful one. This command allows you to manipulate license headers in source code files, and provides four different modes to do so:

1. __add mode__ _(default one)_ - in this mode, _Headroom_ will only add missing license headers, but won't touch existing ones.
1. __replace mode__ - in this mode, _Headroom_ will add missing license headers, but also replace any already existing ones (if differs).


[rel:project-setup-guide]: project-setup-guide