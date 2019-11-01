#' \code{lumper} package
#'
#' Read and write Dockerfiles in R
#'
#' See the README on
#' \href{https://github.com/svdwoude/lumper}{GitHub}
#'
#' @docType package
#' @name lumper
#' @importFrom dplyr %>%
#' @importFrom magrittr %$%
#' @importFrom rlang %||%
#' @importFrom vctrs %0%
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")
  utils::globalVariables(c(
    "."
  ))


