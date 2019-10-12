# Donatron: A library to make accepting donations easy!

An example that shows how to use `Cats' EitherT` for monadic composition & conditional code execution.

The idea is to modify or use`Donatron.donate` to execute code:

```scala
def donate[F[_]: cats.effect.Effect](req: Request): F[Response] =
  checkForValidInts(req)
    .flatMap(checkForMinimumLength)
    .flatMap(submitDonations)
    .flatMap(logValidDonations)
    .flatMap(logAndReturnResponse)
```

The rules for the execution of `donate` function is as follows:

* `checkForValidInts` is the entry point
* `checkForMinimumLength`, `submitDonations` & `logValidDonations` are executed in order.
* The above three functions should only be executed if preceding function was successful.
* `logAndReturnResponse` should be executed regardless of all the previous steps.
* Only exception to `logAndReturnResponse` is `acceptDonations` which can raise an error.