Changes in 0.1.8.0

  * `logGamma` now uses Lancsoz approximation and same as `logGammaL`.  Old
     implementation of `logGamma` moved to `Numeric.SpecFunctions.Extra.logGammaAS245`.

  * Precision of `logGamma` for z<1 improved.

  * New much more precise implementation for `incompleteGamma`

  * Dependency on `erf` pacakge dropped. `erf` and `erfc` just do direct calls
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



Changes in 0.1.7.0

  * Module `statistics: Statistics.Function.Comparison` moved to
    `Numeric.MathFunctions.Comparison`. Old implementation if `within` compared
    negative numbers incorrectly.

  * `addUlps` and `ulpDistance` added to `Numeric.MathFunctions.Comparison`.

  * `relativeError` and `eqRelErr` added to `Numeric.MathFunctions.Comparison`.

  * Precision of `logFactorial` is slightly improved.


Changes in 0.1.6.0

  * `logChoose` added for calculation of logarithm of binomial coefficient

  * `chooseExact` and `logChooseFast` added

  * `sinc` added


Changes in 0.1.5.3

  * Fix for test suite on 32bit platform


Changes in 0.1.5

  * Numeric.Sum: new module adds accurate floating point summation.


Changes in 0.1.4

  * logFactorial type is genberalized. It accepts any `Integral` type

  * Evaluation of polynomials using Horner's method where coefficients
    are store in lists added


Changes in 0.1.3

  * Error function and its inverse added.

  * Digamma function added

  * Evaluation of polynomials using Horner's method added.

  * Crash bug in the inverse incomplete beta fixed.
