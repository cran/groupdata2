

## group
#' @title Create groups from your data
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  Divides data into groups by a wide range of methods.
#'  Creates a grouping factor with \code{1}s for group 1, \code{2}s for group 2, etc.
#'  Returns a \code{data.frame} grouped by the grouping factor for easy use in
#'  \code{magrittr `\%>\%`} pipelines.
#'
#'  By default*, the data points in a group are connected sequentially (e.g. \code{c(1, 1, 2, 2, 3, 3)})
#'  and splitting is done from top to bottom. *Except in the \code{"every"} method.
#'
#'  There are \strong{five} types of grouping methods:
#'
#'  The \code{"n_*"} methods split the data into a given \emph{number of groups}.
#'  They differ in how they handle excess data points.
#'
#'  The \code{"greedy"} method uses a \emph{group size} to split the data into groups,
#'  greedily grabbing \code{`n`} data points from the top.
#'  The last group may thus differ in size (e.g. \code{c(1, 1, 2, 2, 3)}).
#'
#'  The \code{"l_*"} methods use a \emph{list} of either starting points (\code{"l_starts"})
#'  or group sizes (\code{"l_sizes"}). The \code{"l_starts"} method can also auto-detect group starts
#'  (when a value differs from the previous value).
#'
#'  The \code{"every"} method puts every \code{`n`}th data point into the same group
#'  (e.g. \code{c(1, 2, 3, 1, 2, 3)}).
#'
#'  The step methods \code{"staircase"} and \code{"primes"} increase the group size by a step for each group.
#'
#'  \strong{Note}: To create groups balanced by a categorical and/or numerical variable, see the
#'  \code{\link[groupdata2:fold]{fold()}} and \code{\link[groupdata2:partition]{partition()}} functions.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @inheritParams group_factor
#' @param return_factor Only return the grouping factor. (Logical)
#' @param col_name Name of the added grouping factor.
#' @return \code{data.frame} grouped by existing grouping variables and the new grouping factor.
#' @family grouping functions
#' @family staircase tools
#' @family l_starts tools
#' @aliases window binning split
#' @examples
#' # Attach packages
#' library(groupdata2)
#' library(dplyr)
#'
#' # Create data frame
#' df <- data.frame(
#'   "x" = c(1:12),
#'   "species" = factor(rep(c("cat", "pig", "human"), 4)),
#'   "age" = sample(c(1:100), 12)
#' )
#'
#' # Using group()
#' df_grouped <- group(df, n = 5, method = "n_dist")
#'
#' # Using group() in pipeline to get mean age
#' df_means <- df %>%
#'   group(n = 5, method = "n_dist") %>%
#'   dplyr::summarise(mean_age = mean(age))
#'
#' # Using group() with `l_sizes`
#' df_grouped <- group(
#'   data = df,
#'   n = list(0.2, 0.3),
#'   method = "l_sizes"
#' )
#'
#' # Using group_factor() with `l_starts`
#' # `c('pig', 2)` skips to the second appearance of
#' # 'pig' after the first appearance of 'cat'
#' df_grouped <- group(
#'   data = df,
#'   n = list("cat", c("pig", 2), "human"),
#'   method = "l_starts",
#'   starts_col = "species"
#' )
#'
group <- function(data,
                  n,
                  method = "n_dist",
                  starts_col = NULL,
                  force_equal = FALSE,
                  allow_zero = FALSE,
                  return_factor = FALSE,
                  descending = FALSE,
                  randomize = FALSE,
                  col_name = ".groups",
                  remove_missing_starts = FALSE) {
  #
  # Takes data frame or vector
  # Creates a grouping factor
  # If data is a vector
  # .. Return data frame with vector and grouping factor
  # .. grouped by grouping factor
  # If data is a data frame:
  # .. Return data frame grouped by grouping factor
  #

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(x = return_factor, add = assert_collection)
  checkmate::assert_string(x = col_name,
                           min.chars = 1,
                           add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Apply by group (recursion)
  if (dplyr::is_grouped_df(data)) {
    message_once_about_group_by("group")
    group_col_names <- c(colnames(dplyr::group_keys(data)), col_name)
  } else {
    group_col_names <- col_name
  }

  out <- run_by_group_df(
    data = data,
    .fn = run_group_,
    n = n,
    method = method,
    starts_col = starts_col,
    force_equal = force_equal,
    allow_zero = allow_zero,
    return_factor = return_factor,
    descending = descending,
    randomize = randomize,
    col_name = col_name,
    remove_missing_starts = remove_missing_starts
  )

  if (is.data.frame(out)){
    out <- out %>%
      dplyr::group_by(!!!rlang::syms(group_col_names))
  }

  out

}

run_group_ <- function(data,
                       n,
                       method,
                       starts_col,
                       force_equal,
                       allow_zero,
                       return_factor,
                       descending,
                       randomize,
                       col_name,
                       remove_missing_starts) {
  # Create grouping factor
  grouping_factor <- group_factor(
    data = data,
    n = n,
    method = method,
    starts_col = starts_col,
    force_equal = force_equal,
    allow_zero = allow_zero,
    descending = descending,
    randomize = randomize,
    remove_missing_starts = remove_missing_starts
  )

  # If return_factor is set to TRUE
  # .. return the created grouping factor
  if (isTRUE(return_factor)) {
    return(grouping_factor)
  }

  # If force_equal is TRUE
  if (isTRUE(force_equal)) {
    # Shorten data to the length of the grouping factor
    data <- head(data, length(grouping_factor))

  }

  # If data is a vector
  if (!is.data.frame(data)) {
    # Create data frame with data and the grouping factor
    data <- data.frame(data, stringsAsFactors = FALSE)

  }

  # Add grouping factor
  data[[col_name]] <- grouping_factor

  data
}
