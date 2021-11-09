# instance-impl

A GHC plugin that adds a special rust inspired syntax for declaring instances of
[`GHC.Records.HasField`](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Records.html),
for use with
[`-XOverloadedRecordDot`](https://downloads.haskell.org/ghc/9.2.1/docs/html/users_guide/exts/overloaded_record_dot.html)
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
```

will allow you to use `r` as a field with `-XOverloadedRecordDot` just
as you would use `x` and `y`.

You can also use polymorphism, both in the type of the record and the
type of the field:

```
data Pt a = Pt { x, y :: !a }

instance impl (Pt a) where
  x' :: Real a => Double
  x' = realToFrac self.x

  r :: Floating a => a
  r = sqrt $ sq self.x + sq self.y
    where sq v = v*v

  display :: (Show a, IsString str) => str
  display = fromString $ "<" ++ show self.x ++ "," ++ show self.y ++ ">"
```

## Warning

This is still pretty experimental, so things may not work as expected
and the error messages can sometimes be pretty bad. Feel free to
create an issue if something goes wrong.
