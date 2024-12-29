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
#' @title Check which LaTeX software will compile PDF output
#'
#' @description Check which LaTeX software (TinyTeX vs. other LaTeX
#'   distribution) will be used to compile PDF output.
#'
#' @importFrom tinytex is_tinytex tlmgr tlmgr_version
#'
#' @importFrom xfun raw_string
#'
#' @details This function facilitates checking whether the computer running the
#'   script will use TinyTeX or some other LaTeX software to compile PDF files
#'   from LaTeX files. It assumes that if TinyTeX is installed, that will be
#'   used in preference to some alternative LaTeX software (which would be
#'   MiKTeX on CSTAT's virtual server).
#'
#' @return A character value describing which LaTeX software will be used.
#'
#' @examples
#' which_latex()
#'
#' @export
which_latex <- function() {
  UsedTT <- is_tinytex()
  TTInfo <- ifelse(test = UsedTT == TRUE,
                   yes = tlmgr("--version", stdout = TRUE, .quiet = TRUE),
                   no = "Not applicable.")
  x      <- ifelse(test = UsedTT == TRUE,
                   yes = c(paste0("is_tinytex = ", UsedTT, ". We used ",
                                  tlmgr_version(format = "string"), "."),
                           TTInfo),
                   no  = paste0("is_tinytex = ", UsedTT,
                                ". We used other LaTeX software instead."))
  cat(x, sep = '\n')
  if(UsedTT) {tlmgr_version(format = "raw")
  }
}

#===============================================================================
#' @name all_classes
#'
#' @title Combine all classes for an object in one character value
#'
#' @description Converts the vector of classes for an object to a single comma
#'   delimited character value.
#'
#' @param x An object whose classes you want to examine.
#'
#' @details This function uses class(x) to extract the vector of classes for an
#'   object then collapses it into a single comma delimited character value.
#'
#' @return A character value listing the object's classes.
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' library(tibble)
#' x <- tibble(x = 1:3, y = letters[1:3])
#' class(x)
#' all_classes(x)
#'
#' @export
all_classes <- function(x) {
  result <- paste(class(x), collapse = ", ")
  return(result)
}

#===============================================================================
#' @name move_file
#'
#' @title Move files
#'
#' @description Moves files to another location.
#'
#' @param from Character vector, containing source file names or paths to be
#'   moved.
#' @param to Character vector, containing destination file names or paths.
#' @param ... Arguments passed to `file.copy`
#'
#' @details This function is a wrapper for both `file.copy` and `file.remove`
#'   that first copies the source file(s) to the destination, then removes them
#'   from their original location only if all source files were successfully
#'   copied.
#'
#' @export
move_file <- function(from, to, ...) {
  assert_that(class(from) == "character",
              msg = "from and to arguments must be a character value or vector")
  assert_that(class(to) == "character",
              msg = "to argument must be a character value or vector")
  # First copy sources files to the destination
  successful_copy <- file.copy(from = from, to = to, ...)
  N_Tried   <- length(successful_copy)
  N_Copied <- sum(successful_copy)
  N_Failed  <- N_Tried - N_Copied
  # Remove source files only if they were all copied successfully.
  if(all(successful_copy)) file.remove(from)
  # Create result to display to user.
  success_msg <- paste0("Successfully copied all ", N_Tried, " files.")
  failed_msg  <- paste0("Tried to copy ", N_Tried, " files. ",
                        "Succeeded for ", N_Copied, " files. ",
                        "Failed for ", N_Failed, " files. ",
                        "All source files retained.")
  result <- ifelse(test = all(successful_copy),
                   yes = success_msg,
                   no = failed_msg)
  return(result)
}

#===============================================================================
