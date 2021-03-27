## Changes in 0.3.4.2

  * Fixed crash in `invIncompleteBeta` (#68) for some inputs initial approximation
    was computed incorrectly.


## Changes in 0.3.4.1

  * Precision of `incompleteGamma` improved.


## Changes in 0.3.4.0

  * Dependency on `vector-th-unbox` is dropped. All instances are written by
    hand now.


## Changes in 0.3.3.0

  * New implementation for `logGamma`. Now it's precise within 2 ulps at full
    range including zeros at 1 and 2.

  * Bug with precision loss of `invErf` for parameters near zero is fixed.

  * Fixed loss of precision in bundled `erf` near zero. (Affect primarily GHCJS)

  * `factorial` for now uses lookup table

  * `logFactorial` is optimized a bit (less number of terms is used)

  * `m_sqrt_eps` constant added.

  * Module `Numeric.SpecFunctions.Internal` is exposed.

  * Many improvements for test suite. Tables of expected function values are
    generated with mpmath, error estimates improved. Test suite itself is
    migrated to `tasty` from `test-framework`.


## Changes in 0.3.2.1

  * Fixes build on windows for GHC<8.0


## Changes in 0.3.2.0

  * GHCJS is now supported

  * Flag `system-expm1` is set to true by default. Only affects GHC<8.0

## Changes in 0.3.1.0

  * Exported data types for iteration steps in root finding

  * Defaults for root finding algorithm are documented


## Changes in 0.3.0.2

  * Fix license field in cabal file


## Changes in 0.3.0.0

  * `Semigroup` and `Monoid` instances added for data types from `Numeric.Sum`

  * API for finding roots of real functions reworked. 1) All algorithm
    parameters are now tweakable. 2) Functions for getting list of iterations
    added.

  * `Foldable` and `Traversable` instances for `Root` were added.

## Changes in 0.2.1.0

  * `log1p` and `expm1` are simply reexported from `GHC.Float`. They're methods
     of `Floating` type class.

  * On windows `expm1` is implemented in pure haskell for older GHCs.


## Changes in 0.2.0.0

  * Bug fixes and documentation tweaks


## Changes in 0.2.0.0

  * `logGamma` now uses Lancsoz approximation and same as `logGammaL`.  Old
     implementation of `logGamma` moved to `Numeric.SpecFunctions.Extra.logGammaAS245`.

  * Precision of `logGamma` for z<1 improved.

  * New much more precise implementation for `incompleteGamma`

  * Dependency on `erf` package dropped. `erf` and `erfc` just do direct calls
    to C.

  * `Numeric.SpecFunctions.expm1` added

  * `Numeric.SpecFunctions.log1pmx` added.

  * `logGammaCorrection` exported in `Numeric.SpecFunctions.Extra`.

  * Module `Numeric.Series` added for working with infinite sequences, series
    summation and evaluation of continued fractions.

  * Module `statistics: Statistics.Math.RootFinding` copied to
    `Numeric.RootFinding`. Instances for `binary` and `aeson` dropped.

  * Root-finding using Newton-Raphson added

  * `Numeric.MathFunctions.Comparison.ulpDelta` added. It calculates signed
    distance between two doubles.

  * Other bug fixes.



## Changes in 0.1.7.0

  * Module `statistics: Statistics.Function.Comparison` moved to
    `Numeric.MathFunctions.Comparison`. Old implementation if `within` compared
    negative numbers incorrectly.

  * `addUlps` and `ulpDistance` added to `Numeric.MathFunctions.Comparison`.

  * `relativeError` and `eqRelErr` added to `Numeric.MathFunctions.Comparison`.

  * Precision of `logFactorial` is slightly improved.


## Changes in 0.1.6.0

  * `logChoose` added for calculation of logarithm of binomial coefficient

  * `chooseExact` and `logChooseFast` added

  * `sinc` added


## Changes in 0.1.5.3

  * Fix for test suite on 32bit platform


## Changes in 0.1.5

  * Numeric.Sum: new module adds accurate floating point summation.


## Changes in 0.1.4

  * logFactorial type is genberalized. It accepts any `Integral` type

  * Evaluation of polynomials using Horner's method where coefficients
    are store in lists added


## Changes in 0.1.3

  * Error function and its inverse added.

  * Digamma function added

  * Evaluation of polynomials using Horner's method added.

  * Crash bug in the inverse incomplete beta fixed.
