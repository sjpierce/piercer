#'=============================================================================
#' @name tag_um
#'
#' @title Tag user-missing values
#'
#' @description This function converts valid values in an labelled_spss vector
#'   that has a non-NULL na_values attribute to tagged NAs, while retaining
#'   their value labels. This is most useful for numeric variables that have
#'   explicit single-digit user-missing values that are not yet being treated as
#'   NAs in R.
#'
#' @param x A vector of class haven_labelled_spss, such as created by the
#'   labelled_spss() function from the haven package. The vector should
#'   have an na_values attribute listing the values that should be treated as
#'   NAs.
#'
#' @details This function builds on features from the haven package. It is a
#'   convenience function for people who import SPSS data files that used the
#'   discrete missing values feature to mark several values as user-missing
#'   values. SPSS treats them like missing data while preserving distinct value
#'   labels. However, if one reads an SPSS data file in with haven's read_sav()
#'   and the source file had a variable with discrete missing values, those are
#'   identified by the na_values attribute on the variable in the resulting
#'   tibble but are not yet consistently treated as NAs by R. This function
#'   converts the  to tagged NA values.
#'
#' @return A copy of vector x (of class haven_labelled_spss) where the values
#'   specified in ums have been converted to tagged NA values and are now
#'   treated as NAs by R.
#'
#' @importFrom haven labelled_spss
#' @importFrom haven tagged_na
#'
#' @examples
#' library(haven)
#' y <- labelled_spss(x = 1:5, na_values = c(4, 5),
#'                    labels = c(Good = 1, Bad = 3, Unknown = 4, DK = 5))
#' y
#' str(y)
#'
#' # xtabs() shows only non-missing values (expected given default addNA = FALSE).
#' xtabs(~y)
#'
#' # However, setting addNA = TRUE in xtabs() always treats each tagged NA as a
#' # separate category; there's no easy way to collapse them all into just NA.
#' xtabs(~y, addNA = TRUE)
#'
#' # Furthermore, the user-missing values are not correctly handled by other
#' # functions. For example, mean() gives an incorrect result because it still
#' # treats 4 & 5 as valid values. The actual mean of the non-missing values is
#' # 2.
#' mean(y)
#' mean(y, na.rm = TRUE)
#'
#' # Now apply tag_um() to fix these issues.
#' y2 <- tag_um(y)
#' y2
#' print_tagged_na(y2)
#' str(y2)
#'
#' # xtabs() shows only non-missing values (expected given default addNA = FALSE).
#' xtabs(~y2)
#'
#' # If you want to ignore distinctions between tagged NA categories and treat
#' # them all as just NA, we get the correct result this way:
#' xtabs(~y2, addNA = TRUE)
#'
#' # If you want to preserve distinctions between tagged NA categories, we get
#' # the correct result this way:
#' xtabs(~print_tagged_na(y2), addNA = TRUE)
#'
#' # Now mean() will correctly handle the variable with tagged NAs by treating
#' # them all as NAs. It returns NA because it defaults to na.rm = FALSE.
#' mean(y2)
#'
#' # We can get the omit the tagged NA values and get the correct mean.
#' mean(y2, na.rm = TRUE)
#'
#' @export
tag_um <- function (x) {
  # Collect existing variable attributes
  L   <- attr(x, "labels")
  NAV <- attr(x, "na_values")
  # Convert the original NA values vector to tagged NA values.
  TNAV <- tagged_na(letters[1:length(NAV)])
  # Construct revised value labels object.
  LR        <- c(setdiff(L, NAV), TNAV)
  names(LR) <- names(L)
  # Copy v into a new numeric variable x2
  x2 <- x + 0
  # Replace actual user-missing values with the tagged NA values
  for(i in 1:length(NAV)) {
    x2[x2 == NAV[i]] <- TNAV[i]
  }
  # Convert x2 to a labelled_spss variable.
  x2 <- labelled_spss(x = x2, na_values = NAV, labels = LR)
  return(x2)
}

#'=============================================================================

