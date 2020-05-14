_Headroom_ needs to know where to search for configuration, template and source code files. Because of that, you need to perform few setup steps before running it, as described below.

## Automatic Initialization (for OSS projects)
Easiest and fastest way how to initialize Headroom for your project is to use the `init` command, which generates all the boilerplate for you. The only drawback is that you can use it only in case that you use any supported open source license for which Headroom contains license header templates:

```shell
cd project/
headroom init -l bsd3 -s src/
```

This command will automatically scan source code directories for supported file types and will generate:
1. `.headroom.yaml` configuration file with correctly set path to template files, source codes and will contain dummy values for variables.
1. `headroom-templates/` directory which contains template files for all known file types you use in your project and for open source license you choose.

Now the project structure will be following:

```
project/
  ├── src/
  │   ├── scala/
  │   │   ├── Foo.scala
  │   │   └── Bar.scala
  │   └── html/
  │       └── template1.html
  ├── headroom-templates/
  │   ├── html.mustache
  │   └── scala.mustache
  └── .headroom.yaml
```

## Manual Initialization
If you for some reason don't want to use the automatic initialization using the steps above, you can either create all the required files (_YAML_ configuration and template files) by hand, or you can use the `gen` command to do that in semi-automatic way:

```shell
cd project/
headroom gen -c >./.headroom.yaml

mkdir headroom-templates/
cd headroom-templates/

headroom gen -l bsd3:css >./css.mustache
headroom gen -l bsd3:html >./html.mustache
headroom gen -l bsd3:scala >./scala.mustache
```

After these steps, make sure you set correctly the paths to template files and source code files in the _YAML_ configuration file.