In general, _post-processing functions_ are additional functions that can be enabled to perform some post-processing on rendered _license headers_.

Such functions can be enabled and configured in _YAML_ configuration file, under the `post-process` key, where general form of configuration for every _post-processing function_ looks like this:

```yaml
post-process:

FUNCTION_NAME:
    enabled: true       # whether the function is enabled or not
    config:
        KEY1: VALUE1    # some configuration specific to selected function
```

## Available Post-processing Functions
Below is the list of currently implemented _post-processing functions_:

### Update Copyright Years
This functions allows you to update years or years range in rendered license
headers. It uses following rules for updating years:

| Original Value                 | Updated Value                  |
|--------------------------------|--------------------------------|
| Copyright (c) 2019 Author      | Copyright (c) 2019-2020 Author |
| Copyright (c) 2020 Author      | Copyright (c) 2020 Author      |
| Copyright (c) 2017-2019 Author | Copyright (c) 2017-2020 Author |
| Copyright (c) 2017-2020 Author | Copyright (c) 2017-2020 Author |

You don't need to use this exact format of copyright statement
(`Copyright (c) YEAR Author`), as this function uses _regular expressions_ to
find years in entire text of the _license header_. This example also assumes
that the current year is 2020.

#### YAML Configuration

```yaml
post-process:

  update-copyright:
    enabled: true
    config:
     ## If used, updates copyright statements of selected authors only.
     selected-authors-only: ["John Smith"]
```

This _post-processing function_ has single custom configuration key,
`selected-authors-only`. When you ommit this key completely, years in all
copyright statements will be updated. If you need to update statements only
for selected author(s), explicitly state them in the list value of this option.
For example if you use the value from above example, then following
_license header_:

```
Copyright (c) 2019 Rose Tyler
Copyright (c) 2019 John Smith
```

will be updated to

```
Copyright (c) 2019 Rose Tyler
Copyright (c) 2019-2020 John Smith
```