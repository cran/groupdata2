
## %primes%
#' @title Find remainder from 'primes' method
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  When using the \code{"primes"} method,
#'  the last group might not have the size of the associated prime number
#'  if there are not enough elements left. Use \code{\%primes\%} to find this remainder.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param size Size to group (Integer)
#' @param start_at Prime to start at (Integer)
#' @return Remainder (Integer).
#'  Returns \code{0} if the last group
#'  has the size of the associated prime number.
#' @family staircase tools
#' @family remainder tools
#' @aliases primes
#' @examples
#' # Attach packages
#' library(groupdata2)
#'
#' 100 %primes% 2
#' @importFrom dplyr %>%
"%primes%" <- function(size, start_at) {

  #
  # Calculates the remainder (size of last group)
  # when using staircase method on a certain sized vector
  # with a certain step size
  # If the last group has the size of the previous group
  # plus step size, the remainder will be 0.
  #
  # This allows the user to find the right step size,
  # e.g. by looping through step sizes to find a
  # remainder of 0
  #

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_count(x = size, add = assert_collection)
  checkmate::assert_count(x = start_at, positive = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (start_at >= size)
    assert_collection$push("'start_at' must be smaller than 'size'.")
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Get the number of groups with no staircasing
  n_groups <- ceiling(size / start_at)

  # Create a data frame with 1 column containing a group index
  group_data <- data.frame(
    "groups" = seq_len(n_groups),
    stringsAsFactors = FALSE
  )

  # Create a column with number of elements (group number times step size)
  # Create a column with cumulative sum of the number of elements
  group_data <- group_data %>%
    dplyr::mutate(
      n_elements = create_n_primes(length(.data$groups), start_at),
      cumsum = cumsum(as.numeric(.data$n_elements))
    )

  # Get the first row where cumsum is larger or equal to 'size'
  last_group_row <- group_data[group_data[["cumsum"]] >= size, ][1, ]

  # Get the cumulative sum for that group
  # This can be used to calculate excess elements
  cumsum_last_group <- last_group_row[1, 3]

  # Get the cumulative sum for that group
  # This can be used to calculate excess elements
  n_elements_last_group <- last_group_row[1, 2]

  # Get how many excess elements there are
  excess_elements <- cumsum_last_group - size

  # If there are no excess elements
  # it means that the last group has the size of the
  # second last group + step size
  # and the remainder is 0
  if (excess_elements == 0) {
    remainder <- 0
  } else {

    # Get remainder by subtracting the number of excess elements
    # from the number of elements in the last group
    remainder <- n_elements_last_group - excess_elements
  }

  # Return remainder (size of last group)
  remainder
}
