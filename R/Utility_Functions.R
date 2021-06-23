#' @name git_report
#'
#' @title Report on Git Repository Status
#'
#' @description This function produces a short report about whether the current
#'   file is located in a Git repository and, if so, what commit it is at, and
#'   the status of the repository.
#'
#' @param path A character value containing the path to a folder containing a
#'   git repository, defaulting to the result of here::here().
#'
#' @importFrom git2r in_repository repository status
#' @importFrom here here
#' @importFrom utils installed.packages
#' @importFrom assertthat assert_that
#'
#' @details This function facilitates concisely adding version control output
#'   to report scripts. It is basically a wrapper that calls git2r::repository()
#'   and git2r::status() after verifying that you are in a Git repository.
#'
#' @return A data frame.
#'
#' @examples
#' git_report()
#'
#' @export
git_report <- function(path = here()) {
  # Check whether git2r is installed.
  assert_that("git2r" %in% installed.packages(),
              msg = "Install the git2r package to make git_report() work correctly.")

  # If in a git repository, print a summary of the repository & its status

  if (in_repository(path = path) == TRUE) {
    print(repository())
    cat(" \n")
    status()
  }
  else {
    cat("This file is not in a Git repository.\n")
  }
}

#===============================================================================
#' @name which_latex
#'
#' @title Check Which LaTeX Software Will Compile PDF Output
#'
#' @description Check which LaTeX software (TinyTeX vs. other LaTeX
#'   distribution) will be used to compile PDF output.
#'
#' @importFrom tinytex is_tinytex
#'
#' @details This function facilitates checking whether the computer running the
#'   script will use TinyTeX or some other LaTeX software to compile PDF files
#'   from LaTeX files. It assumes that if TinyTeX is installed, that will be
#'   used in preference to some alternative LaTeX software.
#'
#' @return A character value describing which LaTeX software will be used.
#'
#' @examples
#' which_latex()
#'
#' @export
which_latex <- function() {
  UsedTT <- is_tinytex()
  x      <- ifelse(test = UsedTT == TRUE,
                   yes = paste0("is_tinytex = ", UsedTT, ". We used TinyTeX."),
                   no  = paste0("is_tinytex = ", UsedTT,
                                ". We used other LaTeX software instead."))
  return(x)
}

#===============================================================================
