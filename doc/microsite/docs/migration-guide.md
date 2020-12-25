
## Configuration Changes
Following breaking changes needs to be done in your current `.headroom.yaml` configuration file in order to make it compatible with version _v0.4.0.0_:

### Bump Configuration Version
Because this version brings breaking changes to configuration, it's necessary to bump version (`version` field ) or add the field if missing in the `.headroom.yaml` to declare it's compatible with _Headroom_ `v0.4.0.0`:

```yaml
version: 0.4.0.0
```

### Rename `margin-before` and `margin-after` keys

!!! info
    This change is part of [feature #58][github/issue/58], see also [related documentation][doc:configuration#license-headers-key].

Configuration keys `margin-before` and `margin-after` configuration options were renamed as following:

- `margin-before` - must be renamed to `margin-top-code` in all occurences
- `margin-after` - must be renamed to `margin-bottom-code` in all occurences


[doc:v0.3.2.0]: https://doc.norcane.com/headroom/v0.3.2.0/
[github/issueP/58]: https://github.com/vaclavsvejcar/headroom/issues/58

### Change `starts-with`, `ends-with` and `prefixed-by` to regular expressions

!!! info
    This change is part of [feature #61][github/issue/61], see also [related documentation][doc:configuration#license-headers-key].

Configuration keys `starts-with`/`ends-with` (under `block-comment` key) and `prefixed-by` (under `line-comment`) now expects _regular expressions_ instead of plain text. Therefore, if you use any of these keys in your custom configuration, you have to change them to regular expressions. Few examples are below:

- change `/*` to `^\/\*`
- change `*/` to `\*\/$`

## List of all Migration Guides

- [from `v0.3.2.0` to `v0.4.0.0`][v0320-v0400]


[github/issue/58]: https://github.com/vaclavsvejcar/headroom/issues/58
[github/issue/61]: https://github.com/vaclavsvejcar/headroom/issues/61
[doc:configuration#license-headers-key]: documentation/configuration.md#license-headers-key
[v0320-v0400]: https://doc.norcane.com/headroom/v0.4.0.0/migration-guide