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

#' merge Dockerfile
#' @param x string; content Dockerfile x
#' @param y string; content Dockerfile y
#' @importFrom dplyr full_join mutate arrange group_by ungroup select left_join
#' @importFrom purrr map2
#' @importFrom tidyr fill
#' @export
merge_dockerfiles <- function(x, y) {
  # argument can be a raw dockerfile or a parsed dockerfile
  if(is.character(x)) {
    x <- read_dockerfile(x)
  }

  if(is.character(y)) {
    y <- read_dockerfile(y)
  }

  # full join dockeriles by command name and raw content
  merge <- full_join(
    x %>% select(-args),
    y %>% select(-args),
    by=c("name", "raw"),
    suffix = c("_x", "_y")
  ) %>%
    # keep sorting of y to make sure we can keep their order when pushed to x
    arrange(lineno_y, lineno_x) %>%
    # get initial order of x
    mutate(order = lineno_x) %>%
    # give missing y commands same order as subsequent x line
    fill(order) %>%
    # NA order can only happen when it is the inital value, therefore take the following order -1
    mutate(order = ifelse(is.na(order), lead(order) - 1, order)) %>%
    # switch to order x now that y is fitted in
    arrange(lineno_x, lineno_y) %>%
    # give sub order numbers for lines with same order
    group_by(order) %>%
    mutate(order_n = 1:n()) %>%
    ungroup() %>%
    arrange(order, order_n) %>%
    # create column with final natural order
    mutate(lineno = 1:n()) %>%
    # create columns to indicate if command was present in x or y
    mutate(
      x = !is.na(lineno_x),
      y = !is.na(lineno_y),
    ) %>%
    select(-order, -order_n)

  # merge args back in
  merged <- merge %>% left_join(x, by = c("name", "raw", "lineno_x" = "lineno")) %>%
    left_join(y, by = c("name", "raw", "lineno_y" = "lineno")) %>%
    mutate(args = map2(args.x, args.y, ~ .x %||% .y)) %>%
    select(-args.x, -args.y) %>%
    select(lineno, name, args, raw, everything())

  return(merged)
}



