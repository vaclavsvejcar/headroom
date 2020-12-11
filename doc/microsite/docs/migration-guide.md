
## Configuration Changes
Following breaking changes needs to be done in your current `.headroom.yaml` configuration file in order to make it compatible with version _v0.4.0.0_:

### Bump Configuration Version
Because this version brings breaking changes to configuration, it's necessary to bump version (`version` field ) or add the field if missing in the `.headroom.yaml` to declare it's compatible with _Headroom_ `v0.4.0.0`:

```yaml
version: 0.4.0.0
```

### Rename `margin-before` and `margin-after` keys
As part of [this new feature][github/issueP/58], `margin-before` and `margin-after` configuration options were renamed as following:

- `margin-before` - must be renamed to `margin-top-code` in all occurences
- `margin-after` - must be renamed to `margin-bottom-code` in all occurences


[doc:v0.3.2.0]: https://doc.norcane.com/headroom/v0.3.2.0/
[github/issueP/58]: https://github.com/vaclavsvejcar/headroom/issues/58

## List of all Migration Guides

- [from `v0.3.2.0` to `v0.4.0.0`][v0320-v0400]


[v0320-v0400]: https://doc.norcane.com/headroom/v0.4.0.0/migration-guide