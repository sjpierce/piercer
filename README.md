
<!-- README.md is generated from README.Rmd. Please edit that file -->

# piercer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/piercer)](https://CRAN.R-project.org/package=piercer)
<!-- badges: end -->

The goal of [*piercer*](https://github.com/sjpierce/piercer) is to
provide a set of functions and files written for use in my personal
research and statistical collaboration and consulting work. Packaging
them is a learning experience, a way to shorten my other scripts, and a
way to increase quality, reproducibility, and efficiency.

## Task List

  - Add assertthat checks to:
      - *DI()* arguments in `R/Categorical_Data_Functions.R`
      - *rxx.NL()* arguments in `R/NL-SEM_Reliability.R`
      - *p2odds()* arguments in `r/Transform_Functions.R`
      - *p2or()* arguments in `r/Transform_Functions.R`
      - *tag\_um()* arguments in `r/Data_Management_Functions.R`
  - Add more testthat unit tests for various functions:
      - *ci.rpc()*
      - *ci.rp()*
      - *r.ps()*
      - *r.pc()*
      - *r.p*
      - *DI()*
      - *Fisher\_r2z()*
      - *Fisher\_z2r()*
      - *rxx.NL()*
      - *tag\_um()*
      - *geen()*
      - *geep()*

## Installation

This package is not yet available from
[CRAN](https://CRAN.R-project.org). Releasing it there is a long-term
goal. Meanwhile, the development version of *piercer* can be installed
from the source stored in a public [GitHub](https://github.com)
repository using the [*devtools*](https://devtools.r-lib.org/) package.
Precompiled binary installer files are not available at this time, so
installing from the source code is the only option.

### Order of Installation Tasks

Before installing piercer, make sure you have:

-   Installed R 4.1.1 or later. You can get the most recent version of R
    from the [Comprehensive R Archive Network
    (CRAN)](https://cran.r-project.org/).
-   Installed any tools required for compiling packages (they will be
    specific to your operating system). These will be necessary for the
    *devtools* package to work. You may find Bryan & Hester’s (n.d.)
    website useful, especially the [Set up an R dev
    environment](https://rstats.wtf/set-up-an-r-dev-environment.html)
    section.
    -   On Windows, see
        <https://cran.r-project.org/bin/windows/Rtools/>.
    -   On Mac OS X, see <https://cran.r-project.org/bin/macosx/tools>
        and <https://mac.R-project.org/tools/>.
-   Installed the *devtools* package. The most recent [CRAN release of
    *devtools*](https://cran.r-project.org/package=devtools) will likely
    be more stable but sometimes you may instead need the [development
    version at GitHub](https://github.com/r-lib/devtools).
-   Updated your R packages. You may be prompted with a dialog box
    asking “Do you want to install from sources the packages which need
    compilation?” Most of the things work fine if I choose “no”.
    Occasionally, it appears necessary to choose “yes”, but I am more
    likely to run into problems when doing that.  
-   After those tasks are done, you can install *piercer*.

### Install on MS Windows 10 From Source Code on GitHub

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

My normal scenario involves installing the package into an R package
library located on a computer’s own hard drive (e.g., my personal
package library at `C:\Users\Username\Documents\R\win-library\4.1` or to
the main package library at `C:\Program Files\R\R-4.1.1\library`). It is
also possible to install it to a personal package library in a folder
that synchronizes with MS OneDrive such as
`C:\Users\username\OneDrive - Michigan State University\Redirects\Documents\R\win-library\4.1\piercer`.
In the latter case I suggest setting the OneDrive option on that folder
to always retain a local copy on the computer.

At one point I experimented with installing the package to a personal R
package library stored on a Windows network share (i.e., in a user’s
profile that is located by a share name such as
`\\server\Redirected_Folders\username\Documents\R\win-library\4.1`).
That was a bit more cumbersome than I preferred and the performance was
slow, so  
I have omitted instructions for that scenario.

### Install on Apple macOS 10.15 (Catalina) From Source Code on GitHub

Although I use Windows, one of my colleagues successfully installed and
used *piercer* on macOS. We did that around 2020-03-06 shortly after the
release of R 4.0.0. My notes for helping her upgrade to the new major
version of R and re-install *piercer* at that time were as follows. I
have not had any experience with subsequent versions of macOS.

-   Use the following code to save a list of all installed packages
    under old version of R to a file.

    ``` r
    # Save current packages and their versions to an object called ip
    ip <- installed.packages()

    # Save the ip object as an .rds file
    saveRDS(ip, "CurrentPackages.rds")
    ```

-   Uninstall old version of R.

-   Install R 4.0.0 (now I would recommend 4.1.1 instead)

-   Check whether you already have Apple Xcode 10.1. If not, upgrade to
    that version. Note: Xcode 12 appears to be the current stable,
    released version per <https://developer.apple.com/xcode/resources/>.

-   Uninstall Clang 7.0.0

-   Uninstall Fortran 6.1

-   Install Fortran 8.2

-   Use the following code to re-install packages from the list you
    saved to a file.

    ``` r
    # After updating base R, load the file and reinstall
    ip <- readRDS("CurrentPackages.rds")
    install.packages(ip[,1])
    ```

-   Install *piercer* from GitHub.

    ``` r
    devtools::install_github("sjpierce/piercer")
    ```

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

Greenland, S. (2019). Valid p-values behave exactly as they should: Some
misleading criticisms of p-values and their resolution with s-values.
*The American Statistician, 73*(Supplement 1), 106-114.
[doi:10.1080/00031305.2018.1529625](https://doi.org/10.1080/00031305.2018.1529625)

Wasserstein, R. L., Schirm, A. L., & Lazar, N. A. (2019). Moving to a
world beyond “p \< .05”. *The American Statistician, 73*(Supplement 1),
1-19.
[doi:10.1080/00031305.2019.1583913](https://doi.org/10.1080/00031305.2019.1583913)
