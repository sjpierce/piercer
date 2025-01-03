
<!-- README.md is generated from README.Rmd. Please edit that file -->

# piercer

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The goal of [*piercer*](https://github.com/sjpierce/piercer) is to
provide a set of functions and files written for use in my personal
research and statistical collaboration and consulting work. Packaging
them is a learning experience, a way to shorten my other scripts, and a
way to increase quality, reproducibility, and efficiency.

## Installation

This package is not yet available from
[CRAN](https://CRAN.R-project.org). Releasing it there is a long-term
goal. Meanwhile, the development version of *piercer* can be installed
from the source stored in a public [GitHub](https://github.com)
repository using the [*devtools*](https://devtools.r-lib.org/) package.
Precompiled binary installer files are not available at this time, so
installing from the source code is the only option.

### Order of Installation Tasks

Before installing *piercer*, make sure you have:

- Installed R 4.1.0 or later. You can get the most recent version of R
  from the [Comprehensive R Archive Network
  (CRAN)](https://cran.r-project.org/).
- Installed any tools required for compiling packages (they will be
  specific to your operating system). These will be necessary for the
  *devtools* package to work.
  - On Windows, see <https://cran.r-project.org/bin/windows/Rtools/>.
  - On Mac OS X, see <https://cran.r-project.org/bin/macosx/tools> and
    <https://mac.R-project.org/tools/>.
- Installed the *devtools* package. The most recent [CRAN release of
  *devtools*](https://cran.r-project.org/package=devtools) will likely
  be more stable but sometimes you may instead need the [development
  version at GitHub](https://github.com/r-lib/devtools).
- Updated your R packages. You may be prompted with a dialog box asking
  “Do you want to install from sources the packages which need
  compilation?” It usually works fine if I choose “no”. Occasionally, it
  appears necessary to choose “yes”, but I am more likely to run into
  problems when doing that.  
- After those tasks are done, you can install *piercer*.

If you only need *piercer* because a script you want to run loads the
package and uses one of its functions, then type the following code in
the R console to install the package to your package library. You will
need internet access for this to work.

``` r
devtools::install_github("sjpierce/piercer")
```

That should enable you to use the functions built into the package.
Depending on how your computer and R installation are configured, that
may either install R to the main R package library, or to a personal
package library. If you do not have write permissions to the main R
package library, then R will ask if you want to use a personal package
library instead.

## Example

One thing I have been contemplating recently is how to implement some of
the American Statistical Association’s recent recommendations on moving
away from interpreting p-values in terms of statistical significance
(Wasserstein, Schirm, & Lazar, 2019). I found the suggestions to convert
p-values into s-values (Greenland, 2019), Bayes Factor Bounds (BFBs;
Benjamin & Berger, 2019), and posterior probabilities that H1 is true
(Benjamin & Berger, 2019) interesting, so I wrote some functions to do
those conversions.

The example below shows how to apply my *convertp* function. I use one
of the examples for the base R *ttest* function to demonstrate it.

``` r
library(piercer)
tresult <- t.test(1:10, y = c(7:20))
tresult
#> 
#>  Welch Two Sample t-test
#> 
#> data:  1:10 and c(7:20)
#> t = -5.4349, df = 21.982, p-value = 1.855e-05
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -11.052802  -4.947198
#> sample estimates:
#> mean of x mean of y 
#>       5.5      13.5

# Convert p-value into s-value, BFB, & posterior probability that H1 is true. 
convertp(p = tresult$p.value, digits = 3)
#>        S      BFB  PPH1
#> 1 15.718 1820.006 0.999
```

## References

Benjamin, D. J., & Berger, J. O. (2019). Three recommendations for
improving the use of p-values. *The American Statistician,
73*(Supplement 1), 186-191.
[doi:10.1080/00031305.2018.1543135](https://doi.org/10.1080/00031305.2018.1543135)

Bryan, J., & Hester, J. (n.d.). *What they forgot to teach you about R*.
<https://rstats.wtf>

Greenland, S. (2019). Valid p-values behave exactly as they should: Some
misleading criticisms of p-values and their resolution with s-values.
*The American Statistician, 73*(Supplement 1), 106-114.
[doi:10.1080/00031305.2018.1529625](https://doi.org/10.1080/00031305.2018.1529625)

Wasserstein, R. L., Schirm, A. L., & Lazar, N. A. (2019). Moving to a
world beyond “p \< .05”. *The American Statistician, 73*(Supplement 1),
1-19.
[doi:10.1080/00031305.2019.1583913](https://doi.org/10.1080/00031305.2019.1583913)

## Task List

- Add assertthat checks to more functions.
- Add more testthat unit tests for functions.
