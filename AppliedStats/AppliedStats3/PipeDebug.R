#' Mid-pipe assertions
#'
#' Test assertions mid-pipe. Each assertion is executed individually
#' on each group (if present) of the piped data. Any failures indicate
#' the group that caused the fail, terminating on the first failure.
#'
#' If `.debug`, then the interpreter enters the `browser()`, allowing
#' you to look at the specific data, stored as `x` (just the grouped
#' data if `is.grouped_df(.x)`, all data otherwise). If the data is
#' changed, then the altered data will be sent forward in the pipeline
#' (assuming you fixed the failed assertion), otherwise the assertion
#' will fail (as an assertion should).
#'
#' @param .x data.frame, potentially grouped
#' @param ... unnamed expression(s), each must evaluate to a single
#'   'logical'; similar to [assertthat::assert_that()], rather than
#'   combining expressions with `&&`, separate them by commas so that
#'   better error messages can be generated.
#' @param .msg a custom error message to be printed if one of the
#'   conditions is false.
#' @param .debug logical, whether to invoke [browser()] if the
#'   assertion fails; if `TRUE`, then when the debugger begins on a
#'   fail, the grouped data will be in the variable `x`
#' @return data.frame (unchanged)
#' @importFrom assertthat see_if
#' @importFrom dplyr do groups is.grouped_df
#' @export
#' @md
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(assertthat)
#'
#' mtcars %>%
#'   group_by(cyl) %>%
#'   pipe_assert(
#'     all(cyl < 9),
#'     all(mpg > 10)
#'   ) %>%
#'   count()
#' # # A tibble: 3 x 2
#' #     cyl     n
#' #   <dbl> <int>
#' # 1     4    11
#' # 2     6     7
#' # 3     8    14
#' 
#' # note here that the "4" group is processed first and does not fail
#' mtcars %>%
#'   group_by(cyl, vs) %>%
#'   pipe_assert( all(cyl < 6) ) %>%
#'   count()
#' # Error: all(cyl < 6) is not TRUE
#' # Groups: cyl='6', vs='0'
#'
#' }
pipe_assert <- function(.x, ..., .msg = NULL, .debug = FALSE, .groups = NULL) {
  stopifnot(requireNamespace("dplyr"), requireNamespace("assertthat"))
  if (dplyr::is.grouped_df(.x)) {
    .groups <- dplyr::groups(.x)
    return(dplyr::do(.x, pipe_assert(., ..., .msg=.msg, .debug=.debug, .groups=.groups)))
  }
  for (assertion in eval(substitute(alist(...)))) {
    .out <- assertthat::see_if(eval(assertion, .x))
    if (! .out) {
      x <- .x
      if (is.null(.msg)) .msg <- paste(deparse(assertion), "is not TRUE")
      .curgrp <- .pipe_curgrp(x, .groups)
      if (.debug) {
        message(paste("# ", c("", .curgrp, .msg,
                              "",
                              "'x' is the current data that failed the assertion.",
                              "If you make changes to 'x', the pipeline will continue (regardless of the failed assertion).",
                              ""),
                      collapse="\n"))
        browser()
      }
      if (identical(x, .x)) {
        stop(paste(c(.msg, .curgrp), collapse="\n"),
             call.=FALSE)
      } else {
        .x <- x
        return(.x)
      }
    }
  }
  .x # "unmodified"
}

#' Mid-pipe debugging
#'
#' Mid-pipe peek at the data, named `x` within [browser()], and
#' *changes to 'x' are preserved*.
#'
#' @param .x data.frame, potentially grouped
#' @return data.frame
#' @importFrom dplyr do is.grouped_df groups
#' @export
#' @md
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#'
#' mtcars %>%
#'   group_by(cyl, vs) %>%
#'   pipe_debug() %>%
#'   count()
#'
#' }
pipe_debug <- function(x, .groups=NULL) {
  stopifnot(requireNamespace("dplyr"))
  if (dplyr::is.grouped_df(x)) {
    .groups <- dplyr::groups(x)
    return(dplyr::do(x, pipe_debug(., .groups=.groups)))
  }
  .curgrp <- .pipe_curgrp(x, .groups)
  if (is.null(.groups)) {
    character(0)
  } else {
    paste("Groups:",
          paste(.groups, lapply(as.list(select_(x, .dots=.groups)[1,]), sQuote),
                sep="=", collapse=", ")
    )
  }
  message(paste("# ", c("", .curgrp,
                        "",
                        "'x' is the current data, changes will be preserved in the pipeline.",
                        ""),
                collapse="\n"))
  browser()
  x
}

#' Mid-pipe status messaging.
#'
#' @details
#' TODO:
#'
#' - `group_size(.)` works, but `jsonlite::toJSON(list(a=group_size(.)))` does not
#'
#' @param .x data.frame, potentially grouped
#' @param ... unnamed or named expression(s) whose outputs will be
#'   captured, aggregated with [utils::str()], and displayed as a
#'   [base::message()]; if present, a '.' literal is replace with a
#'   reference to the `data.frame` (in its entirety, not grouped)
#' @param .FUN function, typically [message()] or [warning()] (for
#'   when messages are suppressed); note: if set to `warning`, the
#'   argument `call.=FALSE` is appended to the arguments
#' @param .timestamp logical, if 'TRUE' then a POSIXct timestamp is
#'   appended to the header of the `str`-like output (default 'TRUE')
#' @param .stropts optional list of options to pass to [utils::str()],
#'   for example `list(max.level=1)`
#' @return data.frame (unchanged)
#' @importFrom dplyr is.grouped_df
#' @export
#' @md
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#'
#' mtcars %>%
#'   pipe_message(           # unnamed
#'     "starting",
#'     group_size(.)
#'   ) %>%
#'   group_by(cyl) %>%
#'   pipe_message(           # named
#'     msg  = "grouped",
#'     grps = group_size(.)
#'   ) %>%
#'   count() %>%
#'   ungroup() %>%
#'   pipe_message(           # alternate function, for emphasis!
#'     msg = "done",
#'     .FUN = warning
#'   )
#'
#' head(mtcars) %>%
#'   pipe_message(
#'     list(a = list(disp=disp[1], bb=2, cc=3))
#'   )
#' 
#' head(mtcars) %>%
#'   group_by(cyl) %>%
#'   pipe_message(
#'     list(a = list(aa=1, bb=2, cc=3)),
#'     .stropts = list(max.level = 2)
#'   )
#'
#' }
pipe_message <- function(x, ..., .FUN = message, .timestamp = TRUE, .stropts = NULL, .groups = NULL) {
  stopifnot(requireNamespace("dplyr"))
  if (dplyr::is.grouped_df(x)) {
    .groups <- dplyr::groups(x)
    return(dplyr::do(x, pipe_message(., ..., .FUN=.FUN, .timestamp=.timestamp, .stropts=.stropts, .groups=.groups)))
  }
  .expressions <- eval(substitute(alist(...)))
  .curgrp <- .pipe_curgrp(x, .groups)
  lst <- lapply(.expressions, function(.expr) {
    if (is.call(.expr)) .expr <- as.call(lapply(.expr, function(a) if (a == ".") as.symbol("x") else a))
    eval(.expr, x)
  })
  lst <- c(.curgrp, lst)
  .out <- capture.output(
    do.call("str", c(list(lst), .stropts))
  )
  .out[1] <- sprintf("Mid-pipe message%s:",
                     if (.timestamp) paste(" (", Sys.time(), ")", sep = ""))
  do.call(.FUN, c(list(paste(.out, collapse = "\n")),
                  if (identical(.FUN, warning)) list(call. = FALSE)))
  x # "unmodified"
}

# helper function
.pipe_curgrp <- function(x, .groups) {
  if (is.null(.groups)) return(character(0))
  paste("Groups:",
        paste(.groups, lapply(as.list(select_(x, .dots=.groups)[1,]), sQuote),
              sep="=", collapse=", "))
}
