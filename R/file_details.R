#' @name file_details
#'
#' @title File Details
#'
#' @description This function shows details about one or more files in a tibble.
#'
#' @param x A character value (or vector) of file paths.
#'
#' @details This function extracts the file name, size, and date-time last
#'   modified into a tibble.
#'
#' @return A tibble with one row per file path in x containing the file name,
#'   size, and date-time last modified.
#'
#' @importFrom fs file_size
#' @importFrom tibble tibble
#'
#' @export
file_details <- function(x) {
  result <- tibble(File_Name = basename(x),
                   Size = file_size(x),
                   Last_Modified = file.mtime(x))
  return(result)
}

