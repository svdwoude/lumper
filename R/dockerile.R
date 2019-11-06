#' A Dockerfile template
#'
#' @return A dockerfile template
#'
#' @section Methods:
#' \describe{
#'   \item{\code{RUN}}{add a RUN command}
#'   \item{\code{ADD}}{add a ADD command}
#'   \item{\code{COPY}}{add a COPY command}
#'   \item{\code{WORKDIR}}{add a WORKDIR command}
#'   \item{\code{EXPOSE}}{add an EXPOSE command}
#'   \item{\code{VOLUME}}{add a VOLUME command}
#'   \item{\code{CMD}}{add a CMD command}
#'   \item{\code{LABEL}}{add a LABEL command}
#'   \item{\code{ENV}}{add a ENV command}
#'   \item{\code{ENTRYPOINT}}{add a ENTRYPOINT command}
#'   \item{\code{VOLUME}}{add a VOLUME command}
#'   \item{\code{USER}}{add a USER command}
#'   \item{\code{ARG}}{add an ARG command}
#'   \item{\code{ONBUILD}}{add a ONBUILD command}
#'   \item{\code{STOPSIGNAL}}{add a STOPSIGNAL command}
#'   \item{\code{HEALTHCHECK}}{add a HEALTHCHECK command}
#'   \item{\code{STOPSIGNAL}}{add a STOPSIGNAL command}
#'   \item{\code{SHELL}}{add a SHELL command}
#'   \item{\code{MAINTAINER}}{add a MAINTAINER command}
#'   \item{\code{custom}}{add a custom command}
#'   \item{\code{write}}{save the Dockerfile}
#'   \item{\code{switch_cmd}}{switch two command}
#'   \item{\code{remove_cmd}}{remove_cmd one or more command(s)}
#' }
#'
#' @importFrom R6 R6Class
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows pull filter lead mutate n
#' @importFrom glue glue glue_collapse
#' @importFrom rlang is_missing
#' @importFrom stringr str_count
#' @importFrom purrr map
#' @importFrom assertthat assert_that
#' @export
#'
#' @examples
#' my_dock <- Dockerfile$new()

Dockerfile <- R6Class(
  "Dockerfile",
  private = list(
    ..INSTRUCTIONS = c("ADD", "ARG", "CMD", "COPY", "ENTRYPOINT", "ENV", "EXPOSE", "FROM", "LABEL", "MAINTAINER", "ONBUILD", "RUN", "STOPSIGNAL", "USER", "VOLUME", "WORKDIR"),
    ..CMD_REGEX = function() {
      instr <- c(private$..INSTRUCTIONS, "#") %>% glue_collapse(sep = "|")
      regex <- glue("(?:({instr}) +)?(.+)")
      return(regex)
    },
    ..commands = tibble(lineno = integer(0), name = character(0), args = list(), raw = character(0)),
    ..length = function() nrow(private$..commands),
    ..string = function() {
      self$commands %>%
        mutate(
          inline_newlines = str_count(raw, "[^# \\n]+#*(?: {3,}|\\t)[\\w# ]"),
          raw = str_replace_all(raw, "([^# \\n]+)(#*(?: {3,}|\\t)[\\w# ])", "\\1 \\\\\n\\2"),
          newlines = lead(lineno, default = max(lineno) + 1) - lineno - inline_newlines,
          newlines = map_chr(newlines,  ~ rep("\n", max(.x, 1)) %>% glue_collapse()),
          raw = glue("{raw}{newlines}")
        ) %>%
        pull(raw) %>%
        glue_collapse() %>%
        as.character()
    },
    ..append_cmd = function(instr, cmd) {

      # max lineno
      lineno_max <-  max(private$..commands$lineno)
      # last instruction
      instr_last <- private$..commands %>%
        filter(lineno == lineno_max) %>%
        pull(name)
      # decide how many newlines we set between this and the previous command
      newlines <- 2
      if(instr_last == "COMMENT") {
        newlines <- 1
      } else if(instr_last == instr & instr != "RUN") {
        newlines <- 1
      }

      cmd_new <- parse_dockerfile(glue("{instr} {cmd}")) %>%
        mutate(lineno = lineno_max + newlines)

      self$commands <- bind_rows(private$..commands, cmd_new)
    },
    ..prepend_cmd = function(instr, cmd) {

      cmd_new <- parse_dockerfile(glue("{toupper(instr)} {cmd}")) %>%
        mutate(lineno = 1)

      self$commands <- bind_rows(cmd_new, private$..commands %>% mutate(lineno = lineno + 1))
    },
    ..remove_line = function(line) {

      cmds <- private$..commands %>%
        rownames_to_column() %>%
        mutate(newlines = lineno - lag(lineno, default = min(lineno)))

      cmd_remove <- cmds %>%
        filter(lineno == line)

      cmd_newlines <- cmd_remove %>% pull(newlines)
      cmd_row <- cmd_remove %>% pull(rowname) %>% as.integer()

      # replace newlines n+1 with nth newlines value
      self$commands <- cmds %>%
        filter(lineno != line) %>%
        mutate(newlines = ifelse(rowname == (cmd_row + 1), cmd_newlines + 1, newlines)) %>%
        mutate(lineno = cumsum(newlines) + 1)
    },
    ..remove_cmd = function(cmd) {

      lineno <- private$..commands %>%
        filter(raw == cmd) %>%
        pull(lineno)

      private$..remove_line(lineno)
    },
    ..remove_instr = function(instr) {
      if(instr == "#") {
        instr <- "COMMENT"
      }

     cmds <- private$..commands %>%
        filter(name == toupper(instr)) %>%
        pull(raw)

     cmds %>% map(private$..remove_cmd)
    }
  ),
  public = list(
    initialize = function(content, file, commands, FROM = "rocker/r-base", AS){
      if(!is_missing(commands)){
        self$commands <- commands

      } else {
        if (!is_missing(file)) {
          raw <- readChar(file)
        } else if(!is_missing(content)) {
          raw <- content
        } else {
          if (is_missing(AS)) {
            raw <- glue("FROM {FROM}")
          } else {
            raw <- glue("FROM {FROM} AS {AS}")
          }
        }
        self$commands <- parse_dockerfile(raw)
      }
      invisible(self)
    },
    print = function() {
      cat("<Dockerfile> with ", self$length, " command(s)\n", sep = "")
      print(self$commands, n = 5)
    },
    RUN = function(cmd){
      private$..append_cmd("RUN", cmd)
      invisible(self)
    },
    ADD = function(from, to){
      private$..append_cmd("ADD", glue("{from} {to}"))
      invisible(self)
    },
    COPY = function(from, to){
      private$..append_cmd("COPY", glue("{from} {to}"))
      invisible(self)
    },
    WORKDIR = function(where){
      private$..append_cmd("WORKDIR", where)
      invisible(self)
    },
    EXPOSE = function(port){
      assert_that(is.numeric(port))
      assert_that(as.integer(port) == port, msg = "port needs to be an integer")
      port <- as.integer(port)
      assert_that(length(port) == 1)
      private$..append_cmd("EXPOSE", port)
      invisible(self)
    },
    VOLUME = function(volume){
      private$..append_cmd("VOLUME", volume)
      invisible(self)
    },
    CMD = function(cmd){
      private$..append_cmd("CMD", cmd)
      invisible(self)
    },
    LABEL = function(key, value){
      private$..append_cmd("LABEL", glue('"{key}"="{value}"'))
      invisible(self)
    },
    ENV = function(key, value){
      private$..append_cmd("ENV", glue('"{key}"="{value}"'))
      invisible(self)
    },
    ENTRYPOINT = function(cmd){
      private$..append_cmd("ENTRYPOINT", cmd)
      invisible(self)
    },
    USER = function(user){
      private$..append_cmd("USER", user)
      invisible(self)
    },
    ARG = function(arg, ahead = FALSE){
      if (ahead) {
        private$..prepend_cmd("ARG", arg)
      } else {
        private$..append_cmd("ARG", arg)
      }
      invisible(self)
    },
    ONBUILD = function(cmd){
      private$..append_cmd("ONBUILD", cmd)
      invisible(self)
    },
    STOPSIGNAL = function(signal){
      private$..append_cmd("STOPSIGNAL", signal)
      invisible(self)
    },
    HEALTHCHECK = function(check){
      private$..append_cmd("HEALTHCHECK", check)
      invisible(self)
    },
    SHELL = function(shell){
      private$..append_cmd("SHELL", shell)
      invisible(self)
    },
    MAINTAINER = function(name, email){
      private$..append_cmd("MAINTAINER", glue("{name} <{email}>"))
      invisible(self)
    },
    COMMENT = function(comment){
      private$..append_cmd("#", comment)
      invisible(self)
    },
    custom = function(base, cmd){
      private$..append_cmd(base, cmd)
      invisible(self)
    },
    read = function(Dockerfile) {
      self$commands <- parse_dockerfile(Dockerfile)
      return(self)
    },
    write = function(as = "Dockerfile"){
      return(self$string)
    },
    merge = function(Dockerfile) {
      self$commands <- merge_dockerfiles(self$commands, Dockerfile$commands)
      invisible(self)
    },
    remove_cmd = function(cmd, lineno){
      if(!is_missing(lineno)) {
        private$..remove_line(lineno)
      } else {
        private$..remove_cmd(cmd)
      }
      invisible(self)
    },
    remove_instr = function(instr){
      private$..remove_instr(instr)
      invisible(self)
    }
  ),
  active = list(
    commands = function(value) {
      if(is_missing(value)) {
        return(private$..commands)
      } else {
        private$..commands <- value %>% select(lineno, name, args, raw)
      }
    },
    length = function() private$..length(),
    string = function() private$..string()
  )
)


#' parse Dockerfile
#' @param Dockerfile string; content Dockerfile
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom purrr map_chr map
#' @importFrom V8 v8
#' @export
parse_dockerfile <- function(Dockerfile) {
  ct <- v8()
  ct$source(system.file("js/parser.js", package = "lumper"))
  ct$call("parse", Dockerfile,  list(includeComments = TRUE)) %>%
    as_tibble() %>%
    mutate(
      args = map(args, ~ .x),
      raw = ifelse(name == "COMMENT", args, raw),
      raw = map_chr(raw, ~.x)
    )
}




#' merge Dockerfile
#' @param x string; content Dockerfile x
#' @param y string; content Dockerfile y
#' @importFrom dplyr full_join mutate arrange group_by ungroup select left_join everything
#' @importFrom purrr map2
#' @importFrom tidyr fill
#' @export
merge_dockerfiles <- function(x, y) {
  # argument can be a raw dockerfile or a parsed dockerfile
  if(is.character(x)) {
    x <- parse_dockerfile(x)
  }

  if(is.character(y)) {
    y <- parse_dockerfile(y)
  }

  # full join dockeriles by command name and raw content
  commands <- full_join(
      x %>% select(-args),
      y %>% select(-args),
      by=c("name", "raw"),
      suffix = c("_x", "_y")
    ) %>%
    # remove unmatched FROM statements from y
    filter(!(is.na(lineno_x) & name == "FROM")) %>%
    # create columns to indicate if command was present in x or y
    mutate(
      x = !is.na(lineno_x),
      y = !is.na(lineno_y),
    ) %>%
    rownames_to_column()

  prefix_limit <- commands %>%
    filter(name == "FROM") %>%
    pull(rowname) %>%
    as.integer()

  prefix <- commands %>%
    filter(rowname <= prefix_limit) %>%
    select(-rowname)

  commands_ordered <- commands %>%
    filter(rowname > prefix_limit) %>%
    select(-rowname) %>%
    # keep sorting of y to make sure we can keep their order when pushed to x
    arrange(lineno_y, lineno_x) %>%
    bind_rows(prefix, .) %>%
    # get initial order of x
    mutate(order = lineno_x) %>%
    # give missing y commands same order as subsequent x line
    fill(order) %>%
    # NA order can only happen when it is the inital value, therefore take the leading order -1
    mutate(order = ifelse(is.na(order), lead(order) - 1, order)) %>%
    # switch to order x now that y is fitted in
    arrange(lineno_x, lineno_y) %>%
    # give sub order numbers for lines with same order
    group_by(order) %>%
    mutate(order_n = 1:n()) %>%
    ungroup() %>%
    arrange(order, order_n) %>%
    select(-order, -order_n)

    # compute new lineno
   commands_numbered <- commands_ordered %>%
    mutate(order_x = lineno_x, order_y = lineno_y) %>%
    fill(order_x, order_y) %>%
    mutate(
      newlines_x = order_x - lag(order_x, default = min(order_x)),
      newlines_y = order_y - lag(order_y, default = min(order_y)),
      newlines = pmax(newlines_x, newlines_y, na.rm = TRUE),
      lineno = cumsum(newlines) + 1
    ) %>%
    select(-newlines_x, -newlines_y, -order_x, order_y) %>%
    select(lineno, name, raw, newlines, everything())

  # merge args back in
  commands_merged <- commands_numbered %>%
    left_join(x, by = c("name", "raw", "lineno_x" = "lineno")) %>%
    left_join(y, by = c("name", "raw", "lineno_y" = "lineno")) %>%
    mutate(args = map2(args.x, args.y, ~ .x %||% .y)) %>%
    select(-args.x, -args.y) %>%
    select(lineno, name, args, raw)

  return(commands_merged)
}




