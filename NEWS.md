# piercer 0.0.0.9015, 2019-10-13
* Added examples to ci.rpc(), r.pc(), and r.pc() help documentation to resolve 
  GitHub issue #7.

# piercer 0.0.0.9014, 2019-09-21
* Added CRAN badge to `README.Rmd` & `README.md`.

# piercer 0.0.0.9013, 2019-09-21
* Started using roxygen2 import and importFrom tags to replace package::function
  format for calling functions in other packages inside my custom functions. 

# piercer 0.0.0.9012, 2019-09-19
* Added lrcm(). 
* Added brier(). 

# piercer 0.0.0.9011, 2019-09-15
* Fixed bug in PlotCookD() and PlotHat() tht affected row names used in plots. 
* Improved some help documentation. 
* Added ci.rpc(), r.pc(), and r.ps().

# piercer 0.0.0.9010, 2019-09-15
* Added InfCases(), PlotCookD(), and PlotHat(). 

# piercer 0.0.0.9009, 2019-09-14
* Added invlogit() and automated tests. 
* Added hatco() and CookDco(). 

# piercer 0.0.0.9008, 2019-09-14
* Now p2s(), p2bfb(), p2pp, and convertp() return NA when p = NA. 
* Update error messages for p2s(), p2bfb(), p2pp, and convertp(). 
* Updated automated tests for p2s(), p2bfb(), p2pp, and convertp(). 
* Added invlogit() and automated tests. 
* Added hatco() and CookDco(). 

# piercer 0.0.0.9007, 2019-09-12
* Functions p2s(), p2bfb(), p2pp, and convertp() now check for invalid values of
  digits and produce error messages when those checks fail. Added automated 
  tests for verifying that these checks work. 

# piercer 0.0.0.9006, 2019-09-08
* Package now imports stats, haven, and assertthat packages. 
* Started using testthat and assertthat packages to automate testing my 
  functions.
* Functions p2s(), p2bfb(), p2pp, and convertp() now check for invalid values of
  p and produce error messages when those checks fail. Added automated tests for
  verifying that these checks work. 

# piercer 0.0.0.9005, 2019-09-07
* Updated an example for tag_um() to fix GitHub issue #1.
* `DESCRIPTION` file now suggests haven package. 

# piercer 0.0.0.9004, 2019-09-07
* Updated pseudoR2() to fix GitHub issue #2. 

# piercer 0.0.0.9003, 2019-09-05
* Replaced `README.md` (which is now created by knitting `README.Rmd`)

# piercer 0.0.0.9002, 2019-09-04

* Added a `NEWS.md` file to track changes to the package.
* Fixed an example in p2pp() documentation. 

# piercer 0.0.0.9001, 2019-09-04
* Updated title in DESCRIPTION file to work better with citation().

# piercer 0.0.0.9000, 2019-09-02
* Created initial package and R functions.
* Created repository https://github.com/sjpierce/piercer.git
