Changes in 0.1.7.0

  * Module `Statistics.Comparison` moved to `Numeric.MathFunctions.Comparison`.
    Old implementation if `within` compared negative numbers incorretly.

  * `addUlps` and `ulpDistance` added

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
