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


#' parse Dockerfile
#' @param Dockerfile string; content Dockerfile
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr
#' @importFrom V8 v8
#' @export
read_dockerfile <- function(Dockerfile) {
  ct <- v8()
  ct$source(system.file("js/parser.js", package = "lumper"))
  ct$call("parse", Dockerfile,  list(includeComments = TRUE)) %>%
    as_tibble() %>%
    mutate(
      raw = ifelse(name == "COMMENT", args, raw),
      raw = map_chr(raw, ~.x)
    )
}
