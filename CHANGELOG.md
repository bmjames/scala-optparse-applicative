### Version 0.2.1 (26 Nov 2014)

* Fixed implementation of `many` on the `ApplicativePlus[Parser]` instance.

### Version 0.2 (24 Nov 2014)

* Fixed bug reported by Sukant Hajra: `Parser#many` and `Parser#some` had their names
  mixed up.
* Changed the signature of `Parser#some` so that it returns a `scalaz.NonEmptyList`.

