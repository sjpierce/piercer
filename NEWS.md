# News Regarding Changes to piercer

# 0.21.0, 2025-07-05
* `NAMESPACE`
    * Added new function display_num(). 
* Added new function display_num(), with help files. 
    * `man/display_num.Rd`
    * `R/display_num.R`

# 0.20.0, 2025-007-04
* `DESCRIPTION`
    * Updated Imports field.
* `NAMESPACE`
    * Added new function file_details(). 
* Added new function file_details(), with help files. 
    * `man/file_details.Rd`
    * `R/file_details.R`
    
# 0.19.0, 2025-06-20
* `DESCRIPTION`
    * Updated Suggests and Imports fields.
* `NAMESPACE`
    * Added new functions cvv_missingness() and var_missingness(). 
* Added new functions cvv_missingness() and var_missingness(), with help files. 
    * `man/cvv_missingness.Rd`
    * `man/var_missingness.Rd`
    * `R/cvv_missingness.R`
    * `R/var_missingness.R`

## 0.18.0, 2024-12-29
* `DESCRIPTION`
    * Updated RoxygenNote field.
* `piercer.Rproj`
    * Added a ProjectId. 
* `README.Rmd` andf `README.md`
    * Update badges, task list, and explanatory text.
* `R/Utility_Functions.R`
    * Added assertions to move_file() function to check arguments. 

## 0.17.0, 2023-12-07
* Updated `R/Utility_Functions.R` to fix bug in which_latex() caused by changes 
  in tinytex::tlmgr_version().

## 0.16.0, 2023-06-16
* Added `R/SimTools.R` with sim_2arm_prepost() function.
* Updated `DESCRIPTION` and `NAMESPACE` to import additional packages. 
* Updated help files.
    * Updated `man/lrcm.rd`. 
    * Added `man/sim_2arm_prepost.Rd`. 

## 0.15.0, 2023-03-02
* Updated `R/Utility_Functions.R` to add `move_file()` function. 
* Updated `NAMESPACE`
* Added help documentation for `move_file()` to `man/move_file.Rd`. 

## 0.14.0, 2023-02-14
* Updated `R/Utility_Functions.R` to add `all_classes()` function. 
* Updated `NAMESPACE`
* Added help documentation for `all_classes()` to `man/all_classes.Rd`. 
* Added tibble package to `DESCRIPTION` Suggests field. 

## 0.13.0, 2023-01-21
* Updated `R/Utility_Functions.R` to improve `which_latex()` function. 
* Fixed help documentation for lrcm() by documenting the roc parameter.
* Updated RoxygenNote field in `DESCRIPTION`, `NAMESPACE`, and help files via 
  devtools::document().

## 0.12.0, 2022-05-14
* Updated `NAMESPACE` by running devtools::check()
* Updated `R/Correlation_Functions.R` 
    * Fixed assertions used to check arguments for ci.rpc() and ci.rp().
* Updated `tests/testthat/test-ci.rpc.R` to fix an expected value and replace
  comments about test statuses. 
* No errors or warnings from devtools::check() under R 4.2.0. 

## 0.11.0, 2022-03-13
* Updated `R/GLM_Functions.R` to add `extract_glm()` function. 
* Added `man/extract_glm.Rd` help file. 
* Updated `NAMESPACE` file.
* Updated `DESCRIPTION` to add new dependencies on broom and texreg. 

## 0.10.1, 2021-12-12
* Added `tests/testthat/test-ci.rpc.R` to automate unit tests of ci.rpc().
    * 9 tests of ci.rpc() still fail.
    * 4 tests pass. 

## 0.10.0, 2021-12-12
* Updated `R/GEE_Functions.R` to hold source code for:
    * vif_gee_crt() function and associated help. 
* Updated `NAMESPACE` add vif_gee_crt() function.
* Added `man/vif_gee_crt.Rd` to hold help files for vif_gee_crt() function. 
* Updated `man/piercer-package.Rd` description field (changed line breaks). 

## 0.9.1, 2021-09-26
* Updated `DESCRIPTION` to:
    * Revise the description field. 
    * Update the RoxygenNote field. 
* Updated `man/piercer-package.Rd` to reflect changes to `DESCRIPTION`. 
* Updated `README.Rmd` and `README.Rmd` to:
    * Update introductory text. 
    * Update installation instructions. 
    * Fix a spelling error.
    * Update the References section.
    * Update the Task List.

## 0.9.0, 2021-06-23
* Added `R/Utility_Functions.R` to hold source code for:
    * which_latex() function and associated help documentation.
    * git_report() function and associated help documentation.
* Updated `NAMESPACE` file to reflect new functions 
* Added new help files:
    * `man/git_report.Rd`
    * `man/which_latex.Rd`
* Updated `DESCRIPTION` to:
    * List additional suggested and imported packages. 
    * Put list of imported packages in alphabetical order. 
    * Add the VignetteBuilder field. 

## 0.8.4, 2021-06-23
* Fixed tag_um() because devtools::check() revealed an issue in running the 
  example. 

## 0.8.3, 2021-04-07
* Minor update to lrcm() help caused by running check(). 

## 0.8.2, 2020-10-01
* Updated `R/GEE_Functions.R` to:
    * Fix geep() object reference to the p1 argument. 

## 0.8.1, 2020-09-28
* Updated `R/GEE_Functions.R` to:
    * Add assertions to check geen() inputs. 
    * Add assertions to check geep() inputs. 
    * Remove empty examples sections from geen() & geep() help. 
* Updated `README.Rmd` and `README.md` to to update task list. 

## 0.8.0, 2020-09-28
* Updated `R/GEE_Functions.R` to:
    * Add new geen() function and corresponding help documentation. 
    * Fix geen() default arguments. 
    * Fix geen() help documentation. 
* Updates `NAMESPACE`.

## 0.7.0, 2020-09-26
* Updated `R/GEE_Functions.R` to:
    * Add new geen() function and corresponding help documentation. 
    * Fix find_tor_probs() fucntion name in help documentation. 

## 0.6.0, 2020-09-22
* Added `R/GEE_Functions.R` to hold new find_tor_probs() function and additional
  functions yet to be developed. 
* Added `tests/testthat/test-find_tor_probs.R` to automate unit testing for 
  find_tor_probs(). 

## 0.5.1, 2020-09-19
* Updated `R/Data_Management_Functions.R` to fix check() error about tag_um()
  example code "Error: Can't convert <double> to <labelled_spss<integer>>."
  That reauired extensive changes in the function.
* Updated tag_um() help documentation. 

## 0.5.0, 2020-09-17
* Updated `R/Transform_Function.R` to add:
    * p2odds() function and associated help. 
    * p2or() function and associated help. .

## 0.4.0, 2020-05-05
* Updated `README.Rmd` and `README.md` installation instructions.
* Successfully passed devtools::check() and installed via 
  `devtools::install_github("sjpierce/piercer")` under R 4.0.0 on Windows 10. 

## 0.3.8, 2020-04-30
* Upgraded software environment to to R 4.0.0, Rtools 4.0, and RStudio 1.2.5042.

## 0.3.7, 2020-04-28
* Fixed bug in r.p() setting dimnames on sample size matrix N. 

## 0.3.6, 2020-04-28
* Revised rownames in r.p() output for consistency with r.pc() and r.ps(). 
* Fixed bug in r.p() setting dimnames on sample size matrix N. 

## 0.3.5, 2020-04-27
* Fixed r.ps() error message about subscript out of bounds. 
* r.ps() can now extract a subset of the polyserial correlations from the hetcor
  object by specifying cont and/or ord arguments shorter than those used to 
  create the hetcor object. 
* Updated help for r.pc().

## 0.3.4, 2020-04-24
* Improved extraction sample size from a hetcor object in r.p(). 

## 0.3.3, 2020-04-24
* Fixed extraction of sample size in r.p(). 

## 0.3.2, 2020-04-21
* Updated ci.rp() help to say it can be applied to Spearman's rho.
* Updated ci.rp() output to include degrees of freedom.
* Updated r.p() to match new ci.rp() output. 
* Updated `README.Rmd` and `README.md` with better installtion instructions. 

## 0.3.1, 2020-04-14
* Updated ci.rp() to use t-distribution instead of z-distribution for getting 
  p-value. 

## 0.3.0, 2020-03-22
* Added Fisher_r2z(), Fisher_z2r(), ci.rp(), r.p() functions and corresponding 
  help files. 
* Updated ci.rpc() function to use Fisher_r2z() and Fisher_z2r(), and to include
  a conf.level argument. 
* Updated r.pc() and r.ps() to show correlation type in row names.
* Updated r.pc() help file. 

## 0.2.2, 2020-03-17
* Fix warnings generated by PlotCookD() and PlotHat(). 

## 0.2.1, 2020-02-07
* Fixed a bug in lrcm() that was possibly caused by changes in structure of 
  objects created by pROC::ci.coords().
* Updated task list in `README.Rmd` and `README.md`. 
* For rxx.NL() in `R/NL-SEM_Reliability.R`: 
    * Solved check() warning about 'library' or 'require' call to 'pbivnorm' in
      package code. 
    * Solved check() note: rxx.NL: no visible global function definition for 'pbivnorm'.
    * Added *rxx.NL()* help documentation.
    * Improved code indentation and formatting. 
  
## 0.2.0, 2020-02-06
* Added `R/NL-SEM_Reliability.R` script.
* Upated task list in `README.Rmd` and `README.md`. 

## 0.1.3, 2019-12-10
* Added package-level documentation ?piercer. 

## 0.1.2, 2019-12-01
* Fixed a typographical error in p2bfb() help documentation. 

## 0.1.1, 2019-11-27
* Updated help documentation for p2bfb() and lrcm() functions. 

## 0.1.0, 2019-11-03
* Updated `README.Rmd` and `README.md` installation instructions, added links
  to the references, and a link to the package's GitHub repository. 
* This package is now a pre-requisite for other packages I am developing. 
* Tested normal install instructions on a laptop. They worked. Recent experience 
  suggests that installing on a Windows server network share will require more
  steps, but I haven't got a fully tested procedure yet. 

## 0.0.0.9015, 2019-10-13
* Added examples to ci.rpc(), r.pc(), and r.pc() help documentation to resolve 
  GitHub issue #7.

## 0.0.0.9014, 2019-09-21
* Added CRAN badge to `README.Rmd` & `README.md`.

## 0.0.0.9013, 2019-09-21
* Started using roxygen2 import and importFrom tags to replace package::function
  format for calling functions in other packages inside my custom functions. 

## 0.0.0.9012, 2019-09-19
* Added lrcm(). 
* Added brier(). 

## 0.0.0.9011, 2019-09-15
* Fixed bug in PlotCookD() and PlotHat() tht affected row names used in plots. 
* Improved some help documentation. 
* Added ci.rpc(), r.pc(), and r.ps().

## 0.0.0.9010, 2019-09-15
* Added InfCases(), PlotCookD(), and PlotHat(). 

## 0.0.0.9009, 2019-09-14
* Added invlogit() and automated tests. 
* Added hatco() and CookDco(). 

## 0.0.0.9008, 2019-09-14
* Now p2s(), p2bfb(), p2pp, and convertp() return NA when p = NA. 
* Update error messages for p2s(), p2bfb(), p2pp, and convertp(). 
* Updated automated tests for p2s(), p2bfb(), p2pp, and convertp(). 
* Added invlogit() and automated tests. 
* Added hatco() and CookDco(). 

## 0.0.0.9007, 2019-09-12
* Functions p2s(), p2bfb(), p2pp, and convertp() now check for invalid values of
  digits and produce error messages when those checks fail. Added automated 
  tests for verifying that these checks work. 

## 0.0.0.9006, 2019-09-08
* Package now imports stats, haven, and assertthat packages. 
* Started using testthat and assertthat packages to automate testing my 
  functions.
* Functions p2s(), p2bfb(), p2pp, and convertp() now check for invalid values of
  p and produce error messages when those checks fail. Added automated tests for
  verifying that these checks work. 

## 0.0.0.9005, 2019-09-07
* Updated an example for tag_um() to fix GitHub issue #1.
* `DESCRIPTION` file now suggests haven package. 

## 0.0.0.9004, 2019-09-07
* Updated pseudoR2() to fix GitHub issue #2. 

## 0.0.0.9003, 2019-09-05
* Replaced `README.md` (which is now created by knitting `README.Rmd`)

## 0.0.0.9002, 2019-09-04

* Added a `NEWS.md` file to track changes to the package.
* Fixed an example in p2pp() documentation. 

## 0.0.0.9001, 2019-09-04
* Updated title in DESCRIPTION file to work better with citation().

## 0.0.0.9000, 2019-09-02
* Created initial package and R functions.
* Created repository https://github.com/sjpierce/piercer.git
