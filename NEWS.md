# tigerstats 0.3.2

## Major changes

New instructional helpers:

* `make_game()`
* `random_normal_factory()`


## Bug fixes

Console output for `ttestGC()` and similar functions now shows correctly in
R Markdown notebooks.

# tigerstats 0.3.1

## Major changes

New instructional helpers:

* `watch_statisticians()`
* `watch_statisticians_slow()`

# tigerstats 0.3

## Major changes

Further information about the package is now available at:

>http://homerhanumat.github.io/tigerstats

Vignettes have been removed and are now available at the above URL.

`helpGC()` no longer brings vignettes into viewer; instead 
it links to them at the above URL.

# tigerstats 0.2

## Major changes

Addition of vignette "Teaching with Tigerstats."

# tigerstats 0.1.9

## Major changes

* Addition of `qnormGC()`
* Addition of check argument to `lmGC()`.  Diagnostics are no longer done with the `diag` argument; instead one calls the `plot()` function.
* Addition of `polyfitGC()`
* Addition of `henderson` data frame; the `seals` data frame has been re-named to `sealsO2` to avoid name conflict with `seals` in the ggplot2 package.
* Addition of function `helpGC()`, a convenience function to view package vignettes in the R Studio Viewer pane.

# tigerstats 0.1.7

## Major changes

* Addition of package vignettes:  tutorials on all major functions, a summary of descriptive functions and a summary of inferential functions.
* `chisqtestGC()` now provides a graph of resampled chi-square statistics when `simulate.p.value` is set to `TRUE`.
* `barchartGC()` accepts more arguments.

