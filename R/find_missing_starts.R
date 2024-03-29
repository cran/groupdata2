

## find_missing_starts
#' @title Find start positions that cannot be found in \code{`data`}
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Tells you which values and (optionally) \code{skip-to-numbers} that are
#'  recursively removed when using the \code{"l_starts"} method with \code{`remove_missing_starts`}
#'  set to \code{TRUE}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param data \code{data.frame} or \code{vector}.
#'
#'  \strong{N.B.} If \code{`data`} is a \emph{grouped} \code{data.frame},
#'  the function is applied group-wise and the output is a
#'  \code{list} of either \code{vector}s or \code{list}s.
#'  The names are based on the group indices
#'  (see \code{\link[dplyr:group_data]{dplyr::group_indices()}}).
#' @param n List of starting positions.
#'
#'  Skip values by \code{c(value, skip_to_number)} where \code{skip_to_number}
#'  is the nth appearance of the value in the vector.
#'
#'  See \code{\link[groupdata2:group_factor]{group_factor()}} for explanations and
#'  examples of using the \code{"l_starts"} method.
#' @param starts_col Name of column with values to match
#'  when \code{`data`} is a \code{data.frame}. Pass \code{'index'} to use row names. (Character)
#' @param return_skip_numbers Return \code{skip-to-numbers} along with values (Logical).
#' @return List of start values and \code{skip-to-numbers} or a \code{vector} with the start values.
#'  Returns \code{NULL} if no values were found.
#'
#'  \strong{N.B.} If \code{`data`} is a \emph{grouped} \code{data.frame},
#'  the function is applied group-wise and the output is a
#'  \code{list} of either \code{vector}s or \code{list}s.
#'  The names are based on the group indices
#'  (see \code{\link[dplyr:group_data]{dplyr::group_indices()}}).
#' @family l_starts tools
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "a" = c("a", "a", "b", "b", "c", "c"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Create list of starts
#' starts <- c("a", "e", "b", "d", "c")
#'
#' # Find missing starts with skip_to numbers
#' find_missing_starts(df, starts, starts_col = "a")
#'
#' # Find missing starts without skip_to numbers
#' find_missing_starts(df, starts,
#'   starts_col = "a",
#'   return_skip_numbers = FALSE
#' )
find_missing_starts <- function(data,
                                n,
                                starts_col = NULL,
                                return_skip_numbers = TRUE) {
  # Check inputs
  check_find_missing_starts_once(
    data = data,
    n = n,
    starts_col = starts_col,
    return_skip_numbers = return_skip_numbers
  )

  # Apply by group (recursion)
  if (dplyr::is_grouped_df(data)) {
    message_once_about_group_by("find_missing_starts")
  }

  out <- run_by_group_list(
    data = data,
    .fn = run_find_missing_starts_,
    n = n,
    starts_col = starts_col,
    return_skip_numbers = return_skip_numbers
  )

  # Ensure naming is consistent across platforms
  # as Debian GCC seems to have some quirks
  if (is.list(out)){
    out <- purrr::map(out, .f = unname)
    if (!dplyr::is_grouped_df(data)){
      out <- unname(out)
    }
  } else {
    output <- unname(out)
  }

  out
}


run_find_missing_starts_ <- function(data, n, starts_col,
                                     return_skip_numbers) {
  check_find_missing_starts_always(data = data, n = n)

  starts_col <- assign_starts_col(data, starts_col)

  if (is.data.frame(data)) {
    missing <- l_starts_group_factor_(
      v = starts_col,
      n = n,
      remove_missing_starts = FALSE,
      return_missing_starts = TRUE,
      return_missing_starts_skip_numbers = return_skip_numbers
    )
  } else {
    missing <- l_starts_group_factor_(
      v = data,
      n = n,
      remove_missing_starts = FALSE,
      return_missing_starts = TRUE,
      return_missing_starts_skip_numbers = return_skip_numbers
    )
  }

  missing
}

check_find_missing_starts_always <- function(data, n) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  checkmate::assert(
    checkmate::check_data_frame(
      x = data,
      min.cols = 1,
      min.rows = 1
    ),
    checkmate::check_vector(
      x = data,
      min.len = 1,
      strict = TRUE
    ),
    checkmate::check_factor(x = data, min.len = 1),
    .var.name = "data"
  )

  checkmate::reportAssertions(assert_collection)

  # Check number of elements in n
  if (is.data.frame(data)) {
    if (length(n) > nrow(data)) {
      assert_collection$push("'n' cannot have more elements than the number of rows in 'data'.")
    }
  } else if (length(n) > length(data)) {
    assert_collection$push("'n' cannot have more elements than 'data'.")
  }

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

}

check_find_missing_starts_once <-
  function(data,
           n,
           starts_col = NULL,
           return_skip_numbers = TRUE) {
    # Check arguments ####
    assert_collection <- checkmate::makeAssertCollection()
    if (is.null(data)) {
      assert_collection$push("'data' cannot be 'NULL'")
    }
    if (is.null(n)) {
      assert_collection$push("'n' cannot be 'NULL'")
    }
    checkmate::reportAssertions(assert_collection)
    if (length(n) == 1 && all(is.na(n))) {
      assert_collection$push("'n' cannot be 'NA'")
    }
    if (!is.data.frame(data) && length(data) == 1 && is.na(data)) {
      assert_collection$push("'data' cannot be 'NA'.")
    }
    checkmate::assert_flag(x = return_skip_numbers, add = assert_collection)
    checkmate::reportAssertions(assert_collection)

    checkmate::assert(
      checkmate::check_data_frame(
        x = data,
        min.cols = 1,
        min.rows = 1
      ),
      checkmate::check_vector(
        x = data,
        min.len = 1,
        strict = TRUE
      ),
      checkmate::check_factor(x = data, min.len = 1),
      .var.name = "data"
    )
    checkmate::assert(
      checkmate::check_numeric(
        x = n,
        finite = TRUE,
        any.missing = FALSE
      ),
      checkmate::check_character(x = n, any.missing = FALSE),
      checkmate::check_list(
        x = n,
        types = c("character", "numeric", "list"),
        any.missing = FALSE
      ),
      .var.name = "n"
    )

    checkmate::assert(
      checkmate::check_string(
        x = starts_col,
        min.chars = 1,
        null.ok = TRUE
      ),
      checkmate::check_count(
        x = starts_col,
        positive = TRUE,
        null.ok = TRUE
      ),
      .var.name = "starts_col"
    )
    checkmate::reportAssertions(assert_collection)
    if (!is.null(starts_col)) {
      if (!is.data.frame(data)) {
        assert_collection$push("when 'starts_col' is specified, 'data' must be a data frame.")
        checkmate::reportAssertions(assert_collection)
      }
      if (checkmate::test_string(starts_col) &&
          starts_col %ni% c(colnames(data), "index", ".index")) {
        assert_collection$push(paste0(
          "'starts_col' column, '",
          starts_col,
          "', not found in 'data'."
        ))
      } else if (checkmate::test_number(starts_col) &&
                 ncol(data) < starts_col) {
        assert_collection$push(
          paste0(
            "'starts_col' was passed as a column index but was larger th",
            "an the number of columns in 'data'."
          )
        )
      }
      checkmate::reportAssertions(assert_collection)

      if (checkmate::test_number(starts_col))
        starts_col <- colnames(data)[[starts_col]]

      checkmate::assert(
        checkmate::check_string(x = starts_col, pattern = "^\\.?index$"),
        checkmate::check_numeric(x = data[[starts_col]]),
        checkmate::check_character(x = data[[starts_col]]),
        checkmate::check_factor(x = data[[starts_col]]),
        .var.name = "data[[starts_col]]"
      )
    } else if (is.data.frame(data)) {
      assert_collection$push("when 'data' is a data frame, 'starts_col' must be specified.")
    }

    # Check number of elements in n
    if (is.data.frame(data)) {
      if (length(n) > nrow(data)) {
        assert_collection$push("'n' cannot have more elements than the number of rows in 'data'.")
      }
    } else if (length(n) > length(data)) {
      assert_collection$push("'n' cannot have more elements than 'data'.")
    }

    checkmate::reportAssertions(assert_collection)
    # End of argument checks ####
  }
