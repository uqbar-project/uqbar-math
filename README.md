uqbar-math [![Build Status](https://travis-ci.org/uqbar-project/uqbar-math.svg?branch=master)](https://travis-ci.org/uqbar-project/uqbar-math)
==========

This library extends *Scala* in order to provide better abstractions to support complex algebraic expressions.  

Setup
-----

To include this library in your *SBT* project, just add the following lines to your `build.sbt` file:

```scala
resolvers += "Uqbar Central" at "http://uqbar-wiki.org/mvn/releases"

libraryDependencies += "org.uqbar" %% "uqbar-math" % "latest.integration"
```

Features
--------
###Vector

***Vectors*** are basic 2D structures which can be used to represent positions, translations or forces.

Two Vector implementations are provided:

* **Vector**: Exposes all basic operations.
* **MutableVector**: Extends `Vector` with mutable operations.

Note that `Vector` **does NOT** guarantee to be *immutable*, just doesn't expose the messages to be mutated.
This is so to provide a more flexible approach, and allow mutable vector owners to expose them in a secure way.
You can find out more about it [here](http://mycodeofhonor.blogspot.com.ar/2013/05/scaling-into-position.html).

Implicit conversions to ease code writing are defined in `org.uqbar.math.versors` package.

```scala
import org.uqbar.math.vector._

val here: Vector = (1,5)                        // Vector converted from tuple
val there = here + (2.5,10)                     // Also a vector
val distance = here manhattanDistanceTo there

```

Contributions
-------------

Yes, please! Pull requests are always welcome, just try to keep it small and clean.

Version History
---------------

#### 1.1
* Added bidirectional AWT Point and Tuple implicit conversions.
* Added `Vector.random()`.
* Many small SBT files instead of huge one.
* Fixed some vector bugs.

License
-------

This code is open source software licensed under the [LGPL v3 License](https://www.gnu.org/licenses/lgpl.html) by [The Uqbar Foundation](http://www.uqbar-project.org/). Feel free to use it accordingly.
