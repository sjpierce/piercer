
<!-- README.md is generated from README.Rmd. Please edit that file -->

# piercer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/piercer)](https://CRAN.R-project.org/package=piercer)
<!-- badges: end -->

The goal of [piercer](https://github.com/sjpierce/piercer) is to provide
a set of functions and files written for use in my personal research and
statistical consulting work. Packaging them is a learning experience, a
way to shorten my other scripts, and a way to increase quality,
reproducibility, and efficiency.

## Installation

This package is not yet available from
[CRAN](https://CRAN.R-project.org). Releasing it there is a long-term
goal. Meanwhile, the development version can be installed from
[GitHub](https://github.com) using the *devtools* package.

### Normal Install

A normal install scenario involves installing the package into an R
package library located on a computer’s own hard drive (e.g.,
**C:/Users/Username/Documents/R/win-library/3.6** or **C:/Program
Files/R/R-3.6.1/library**) instead of on a Windows network share.

In a normal install scenario, use the following code. If you don’t have
*devtools* installed, uncomment that line first. It is possible that you
also need to install the latest recommended version of
[Rtools](https://cran.r-project.org/bin/windows/Rtools) first.

``` r
# install.packages("devtools")
devtools::install_github("sjpierce/piercer")
```

### Install on a Windows Network Share

Instructions to be written later. The process is more involved when you
intend to install the package to a personal R package library that is
stored on a Windows network share (i.e., in a user’s profile that is
located by a share name such as
**\\\\\\\\server/Redirected\_Folders/username/Documents/R/win-library/3.6**).

## Example

One thing I have been contemplating recently is how to implement some of
the American Statitical Association’s recent recommendations on moving
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

Greenland, S. (2019). Valid p-values behave exactly as they should: Some
misleading criticisms of p-values and their resolution with s-values.
*The American Statistician, 73*(Supplement 1), 106-114.
[doi:10.1080/00031305.2018.1529625](https://doi.org/10.1080/00031305.2018.1529625)

Wasserstein, R. L., Schirm, A. L., & Lazar, N. A. (2019). Moving to a
world beyond “p \< .05”. *The American Statistician, 73*(Supplement 1),
1-19.
[doi:10.1080/00031305.2019.1583913](https://doi.org/10.1080/00031305.2019.1583913)
