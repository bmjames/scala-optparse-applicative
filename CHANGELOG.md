### Version 0.6 (21 Dec 2016)
* Remove dependency on Kiama (Colt Frederickson)

### Version 0.5 (5 Nov 2016)
* Add support for Scala 2.12 (Rob Norris)

### Version 0.4 (27 July 2016)

* Updated Scalaz version to 7.2.4 (Kris Nuttycombe, Sam Roberts)
* Updated Scala 2.11 series version to 2.11.8 (Sam Roberts)
* Add footer builders to API (Maun Suang Boey)

### Version 0.3 (13 Oct 2015)

* `ReadM` and `Parser` instances for more Scala standard library types (Adelbert Chang).
* Updated Scalaz version to 7.1.4, and Scala cross-compilation targets to 2.10.6 and 2.11.7 (Colt Frederickson).

### Version 0.2.1 (26 Nov 2014)

* Fixed implementation of `many` on the `ApplicativePlus[Parser]` instance.

### Version 0.2 (24 Nov 2014)

* Fixed bug reported by Sukant Hajra: `Parser#many` and `Parser#some` had their names
  mixed up.
* Changed the signature of `Parser#some` so that it returns a `scalaz.NonEmptyList`.
