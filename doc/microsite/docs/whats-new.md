## New & Noteworthy

### Extended margin options for license headers
In previous version of _Headroom_, if margin (empty lines) around generated header is needed, one can use the `margin-before` and `margin-after` config properties. These allow to define margin between header and preceding/following code, but aren't applied when header is very first (or very last) thing in the source code. This can cause trouble for example when keeping blank line after header, which is last thing in the file, is required.

In order to make this more flexible, current two configuration options are dropped and replaced by following ones:

- `margin-top-code` - defines margin (in no. of empty lines) between generated header and code above it, but only **IF** header is **NOT** the first thing in file.
- `margin-top-file` - defines margin (in no. of empty lines) between generated header the very top of the file, but only **IF** header **IS** the first thing in file.
- `margin-bottom-code` - defines margin (in no. of empty lines) between generated header and code below it, but only **IF** header is **NOT** the last thing in file.
- `margin-bottom-file` - defines margin (in no. of empty lines) between generated header the very end of the file, but only **IF** header **IS** the last thing in file.

#### Examples

```haskell
                               -- <<< 'margin-top-file' set to '1'
{- some header -}
                               -- <<< 'margin-bottom-code' set to '2'

someFunc :: String
someFunc = undefined
```