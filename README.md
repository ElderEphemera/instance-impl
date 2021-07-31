# instance-impl

A GHC plugin that adds a special rust inspired syntax for declaring instances of
[`GHC.Records.HasField`](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Records.html),
for use with
[`-XOverloadedRecordDot`](https://downloads.haskell.org/ghc/9.2.1-alpha1/docs/html/users_guide/exts/overloaded_record_dot.html)
(introduced in GHC 9.2).

See [this blog
post](https://www.parsonsmatt.org/2021/07/29/stealing_impl_from_rust.html) by
Matt Parsons for what inspired this.

## Example

With this plugin enabled (using `-fplugin=InstanceImpl`), this code:

```
data Pt = Pt { x, y :: !Double }

instance impl Pt where
  r :: Double
  r = sqrt $ sq self.x + sq self.y
    where sq v = v*v

  θ :: Double
  θ = atan2 self.y self.x
```

will allow you to use `r` and `θ` as fields with `-XOverloadedRecordDot` just as
you would use `x` and `y`.

## Limitations

Currently you cannot have fields with polymorphic types.
