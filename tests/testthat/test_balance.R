library(groupdata2)
context("balance()")

test_that("all size settings work in balance()", {
  xpectr::set_test_seed(1)

  # Create data frame
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "score" = sample(c(1:100), 7)
  )

  # Using balance() with number
  xpectr::set_test_seed(1)
  df_3 <- balance(df, 3, "participant")

  expect_equal(nrow(df_3), 3 * 3)
  expect_equal(df_3$participant, factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3)))
  expect_equal(df_3$trial, c(1, 2, 1, 1, 1, 1, 4, 1, 2))
  expect_equal(ncol(df_3), 3)


  # Using balance() with min
  xpectr::set_test_seed(2)
  df_min <- balance(df, "min", "participant")

  expect_equal(nrow(df_min), 3)
  expect_equal(df_min$participant, factor(c(1, 2, 3)))
  expect_equal(df_min$trial, c(1, 1, 3))
  expect_equal(ncol(df_min), 3)

  # Using balance() with max
  xpectr::set_test_seed(2)
  df_max <- balance(df, "max", "participant")

  expect_equal(nrow(df_max), 4 * 3)
  expect_equal(df_max$participant, factor(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3)))
  expect_equal(df_max$trial, c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 3, 4))
  expect_equal(ncol(df_max), 3)

  # Using balance() with mean
  xpectr::set_test_seed(19)
  df_mean <- balance(df, "mean", "participant")

  expect_equal(nrow(df_mean), 2 * 3)
  expect_equal(df_mean$participant, factor(c(1, 1, 2, 2, 3, 3)))
  expect_equal(df_mean$trial, c(1, 2, 1, 1, 2, 4))
  expect_equal(ncol(df_mean), 3)

  # Using balance() with median
  xpectr::set_test_seed(19)
  df_median <- balance(df, "median", "participant")

  expect_equal(nrow(df_median), 2 * 3)
  expect_equal(df_median$participant, factor(c(1, 1, 2, 2, 3, 3)))
  expect_equal(df_median$trial, c(1, 2, 1, 1, 2, 4))
  expect_equal(ncol(df_median), 3)

  # Errors

})

test_that("fuzz testing balance()", {

  xpectr::set_test_seed(1)

  # Create data frame
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "diagnosis" = c(1,1,2,2,2,2,2),
    "score" = sample(c(1:100), 7)
  )

  xpectr::set_test_seed(19)
  # xpectr::gxs_function(
  #   balance,
  #   args_values = list(
  #     "data" = list(df, df %>% dplyr::mutate(participant = as.character(participant)),
  #                   NA, 1, matrix(1, nrow = 3, ncol = 2)),
  #     "size" = list("max", 2,-3, 0, "moon", NA),
  #     "id_col" = list("participant", "hej", NA),
  #     "cat_col" = list("diagnosis", "trial", "participant", "hej", NA, 2),
  #     "id_method" = list("n_ids", "n_rows_c", "distributed", "nested", "nope", 1, NA),
  #     "mark_new_rows" = list(FALSE, TRUE, NA, 1, "TRUE"),
  #     "new_rows_col_name" = list(".new_row", FALSE, 1, NA)
  #   ),
  #   indentation = 2
  # )


  ## Testing 'balance'                                                        ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing balance(data = df, size = "max", id_col = "par...
  xpectr::set_test_seed(42)
  # Assigning output
  output_11171 <- balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")
  # Testing class
  expect_equal(
    class(output_11171),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_11171[["participant"]],
    structure(c(1L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  expect_equal(
    output_11171[["trial"]],
    c(1, 1, 2, 2, 1, 1, 2, 3, 4),
    tolerance = 1e-4)
  expect_equal(
    output_11171[["diagnosis"]],
    c(1, 1, 1, 1, 2, 2, 2, 2, 2),
    tolerance = 1e-4)
  expect_equal(
    output_11171[["score"]],
    c(27, 27, 37, 37, 57, 89, 20, 86, 97),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_11171),
    c("participant", "trial", "diagnosis", "score"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_11171),
    c("factor", "numeric", "numeric", "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_11171),
    c("integer", "double", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_11171),
    c(9L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_11171)),
    character(0),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "trial", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * The value in 'data[[cat_col]]' must",
                         " be constant within each ID.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "participant", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip("1 assertions failed:\n * 'id_col' and 'cat_col' cannot contain the same column name."),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "hej", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip("1 assertions failed:\n * 'cat_col' column(s), 'hej', not found in 'data'."),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = NA, id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'cat_col': Contains missin",
                         "g values (element 1).")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = 2, id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'cat_col': Must be of type",
                         " 'character', not 'double'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: cat_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = NULL, id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'cat_col': Must be of type",
                         " 'character', not 'NULL'.")),
    fixed = TRUE)

  # Testing balance(data = df %>% dplyr::mutate(participan...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df %>% dplyr::mutate(participant = as.character(participant)), size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data[[id_col]]': Must be ",
                         "of type 'factor', not 'character'.")),
    fixed = TRUE)

  # Testing balance(data = NA, size = "max", id_col = "par...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = NA, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type 'd",
                         "ata.frame', not 'logical'.")),
    fixed = TRUE)

  # Testing balance(data = 1, size = "max", id_col = "part...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = 1, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type 'd",
                         "ata.frame', not 'double'.")),
    fixed = TRUE)

  # Testing balance(data = matrix(1, nrow = 3, ncol = 2), ...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = matrix(1, nrow = 3, ncol = 2), size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type 'd",
                         "ata.frame', not 'matrix'.")),
    fixed = TRUE)

  # Testing balance(data = NULL, size = "max", id_col = "p...
  # Changed from baseline: data
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = NULL, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'data': Must be of type 'd",
                         "ata.frame', not 'NULL'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "hej...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "hej", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip("1 assertions failed:\n * 'id_col' column, 'hej', not found in 'data'."),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = NA, ...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = NA, cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip("1 assertions failed:\n * Variable 'id_col': May not be NA."),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = NULL...
  # Changed from baseline: id_col
  xpectr::set_test_seed(42)
  # Assigning output
  output_12282 <- balance(data = df, size = "max", id_col = NULL, cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")
  # Testing class
  expect_equal(
    class(output_12282),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_12282[["participant"]],
    structure(c(1L, 1L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  expect_equal(
    output_12282[["trial"]],
    c(1, 2, 2, 2, 1, 1, 1, 2, 3, 4),
    tolerance = 1e-4)
  expect_equal(
    output_12282[["diagnosis"]],
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    tolerance = 1e-4)
  expect_equal(
    output_12282[["score"]],
    c(27, 37, 37, 37, 27, 57, 89, 20, 86, 97),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_12282),
    c("participant", "trial", "diagnosis", "score"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_12282),
    c("factor", "numeric", "numeric", "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_12282),
    c("integer", "double", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_12282),
    c(10L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_12282)),
    character(0),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: id_method
  xpectr::set_test_seed(42)
  # Assigning output
  output_19049 <- balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_rows_c", mark_new_rows = FALSE, new_rows_col_name = ".new_row")
  # Testing class
  expect_equal(
    class(output_19049),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19049[["participant"]],
    structure(c(1L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  expect_equal(
    output_19049[["trial"]],
    c(1, 1, 2, 2, 1, 1, 2, 3, 4),
    tolerance = 1e-4)
  expect_equal(
    output_19049[["diagnosis"]],
    c(1, 1, 1, 1, 2, 2, 2, 2, 2),
    tolerance = 1e-4)
  expect_equal(
    output_19049[["score"]],
    c(27, 27, 37, 37, 57, 89, 20, 86, 97),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_19049),
    c("participant", "trial", "diagnosis", "score"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19049),
    c("factor", "numeric", "numeric", "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19049),
    c("integer", "double", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19049),
    c(9L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19049)),
    character(0),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: id_method
  xpectr::set_test_seed(42)
  # Assigning output
  output_13975 <- balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "distributed", mark_new_rows = FALSE, new_rows_col_name = ".new_row")
  # Testing class
  expect_equal(
    class(output_13975),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_13975[["participant"]],
    structure(c(1L, 1L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  expect_equal(
    output_13975[["trial"]],
    c(1, 2, 2, 1, 2, 1, 1, 2, 3, 4),
    tolerance = 1e-4)
  expect_equal(
    output_13975[["diagnosis"]],
    c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    tolerance = 1e-4)
  expect_equal(
    output_13975[["score"]],
    c(27, 37, 37, 27, 37, 57, 89, 20, 86, 97),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_13975),
    c("participant", "trial", "diagnosis", "score"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_13975),
    c("factor", "numeric", "numeric", "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_13975),
    c("integer", "double", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_13975),
    c(10L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_13975)),
    character(0),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: id_method
  xpectr::set_test_seed(42)
  # Assigning output
  output_15676 <- balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "nested", mark_new_rows = FALSE, new_rows_col_name = ".new_row")
  # Testing class
  expect_equal(
    class(output_15676),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_15676[["participant"]],
    structure(c(1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  expect_equal(
    output_15676[["trial"]],
    c(1, 2, 1, 1, 1, 1, 1, 2, 3, 4),
    tolerance = 1e-4)
  expect_equal(
    output_15676[["diagnosis"]],
    c(1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
    tolerance = 1e-4)
  expect_equal(
    output_15676[["score"]],
    c(27, 37, 57, 57, 57, 57, 89, 20, 86, 97),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_15676),
    c("participant", "trial", "diagnosis", "score"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_15676),
    c("factor", "numeric", "numeric", "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_15676),
    c("integer", "double", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_15676),
    c(10L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_15676)),
    character(0),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: id_method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "nope", mark_new_rows = FALSE, new_rows_col_name = ".new_row"), lowercase = TRUE),
    xpectr::strip(
      ifelse(is_checkmate_v2_1(),
             "must be a subset of {n_ids,n_rows_c,distributed,nested}",
             "must be a subset of set {n_ids,n_rows_c,distributed,nested}.")
      , lowercase = TRUE),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: id_method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = 1, mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_method': Must be of ty",
                         "pe 'string', not 'double'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: id_method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = NA, mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip("1 assertions failed:\n * Variable 'id_method': May not be NA."),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: id_method
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = NULL, mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'id_method': Must be of ty",
                         "pe 'string', not 'NULL'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: mark_new_rows
  xpectr::set_test_seed(42)
  # Assigning output
  output_18115 <- balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = TRUE, new_rows_col_name = ".new_row")
  # Testing class
  expect_equal(
    class(output_18115),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_18115[["participant"]],
    structure(c(1L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  expect_equal(
    output_18115[["trial"]],
    c(1, 1, 2, 2, 1, 1, 2, 3, 4),
    tolerance = 1e-4)
  expect_equal(
    output_18115[["diagnosis"]],
    c(1, 1, 1, 1, 2, 2, 2, 2, 2),
    tolerance = 1e-4)
  expect_equal(
    output_18115[["score"]],
    c(27, 27, 37, 37, 57, 89, 20, 86, 97),
    tolerance = 1e-4)
  expect_equal(
    output_18115[[".new_row"]],
    c(0, 1, 0, 1, 0, 0, 0, 0, 0),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_18115),
    c("participant", "trial", "diagnosis", "score", ".new_row"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_18115),
    c("factor", "numeric", "numeric", "integer", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_18115),
    c("integer", "double", "double", "integer", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_18115),
    c(9L, 5L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_18115)),
    character(0),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: mark_new_rows
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = NA, new_rows_col_name = ".new_row")),
    xpectr::strip("1 assertions failed:\n * Variable 'mark_new_rows': May not be NA."),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: mark_new_rows
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = 1, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'mark_new_rows': Must be o",
                         "f type 'logical flag', not 'double'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: mark_new_rows
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = "TRUE", new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'mark_new_rows': Must be o",
                         "f type 'logical flag', not 'character'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: mark_new_rows
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = NULL, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'mark_new_rows': Must be o",
                         "f type 'logical flag', not 'NULL'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: new_rows_col_name
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = FALSE)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'new_rows_col_name': Must ",
                         "be of type 'string', not 'logical'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: new_rows_col_name
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = 1)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'new_rows_col_name': Must ",
                         "be of type 'string', not 'double'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: new_rows_col_name
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = NA)),
    xpectr::strip("1 assertions failed:\n * Variable 'new_rows_col_name': May not be NA."),
    fixed = TRUE)

  # Testing balance(data = df, size = "max", id_col = "par...
  # Changed from baseline: new_rows_col_name
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "max", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = NULL)),
    xpectr::strip(paste0("1 assertions failed:\n * Variable 'new_rows_col_name': Must ",
                         "be of type 'string', not 'NULL'.")),
    fixed = TRUE)

  # Testing balance(data = df, size = 2, id_col = "partici...
  # Changed from baseline: size
  xpectr::set_test_seed(42)
  # Assigning output
  output_16237 <- balance(data = df, size = 2, id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")
  # Testing class
  expect_equal(
    class(output_16237),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_16237[["participant"]],
    structure(c(1L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 3L), .Label = c("1",
      "2", "3"), class = "factor"))
  expect_equal(
    output_16237[["trial"]],
    c(1, 1, 2, 2, 1, 1, 2, 3, 4),
    tolerance = 1e-4)
  expect_equal(
    output_16237[["diagnosis"]],
    c(1, 1, 1, 1, 2, 2, 2, 2, 2),
    tolerance = 1e-4)
  expect_equal(
    output_16237[["score"]],
    c(27, 27, 37, 37, 57, 89, 20, 86, 97),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_16237),
    c("participant", "trial", "diagnosis", "score"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_16237),
    c("factor", "numeric", "numeric", "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_16237),
    c("integer", "double", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_16237),
    c(9L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_16237)),
    character(0),
    fixed = TRUE)

  # Testing balance(data = df, size = -3, id_col = "partic...
  # Changed from baseline: size
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = -3, id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * 'size' must be one of 'min','max','",
                         "mean','median' or a positive whole number.")),
    fixed = TRUE)

  # Testing balance(data = df, size = 0, id_col = "partici...
  # Changed from baseline: size
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = 0, id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * 'size' must be one of 'min','max','",
                         "mean','median' or a positive whole number.")),
    fixed = TRUE)

  # Testing balance(data = df, size = "moon", id_col = "pa...
  # Changed from baseline: size
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = "moon", id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * 'size' must be one of 'min','max','",
                         "mean','median' or a positive whole number.")),
    fixed = TRUE)

  # Testing balance(data = df, size = NA, id_col = "partic...
  # Changed from baseline: size
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = NA, id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * 'size' must be one of 'min','max','",
                         "mean','median' or a positive whole number.")),
    fixed = TRUE)

  # Testing balance(data = df, size = NULL, id_col = "part...
  # Changed from baseline: size
  xpectr::set_test_seed(42)
  # Testing side effects
  expect_error(
    xpectr::strip_msg(balance(data = df, size = NULL, id_col = "participant", cat_col = "diagnosis", id_method = "n_ids", mark_new_rows = FALSE, new_rows_col_name = ".new_row")),
    xpectr::strip(paste0("1 assertions failed:\n * 'size' must be one of 'min','max','",
                         "mean','median' or a positive whole number.")),
    fixed = TRUE)

  ## Finished testing 'balance'                                               ####
  #

})


test_that("mark_new_rows works in balance()", {
  xpectr::set_test_seed(1)

  # Create data frame
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "score" = sample(c(1:100), 7)
  )

  xpectr::set_test_seed(1)
  df_3 <- balance(df, 3, "participant", mark_new_rows = TRUE)
  expect_equal(df_3$.new_row, c(0, 0, 1, 0, 1, 1, 0, 0, 0))

  xpectr::set_test_seed(1)
  df_3 <- balance(df, 3, "participant", mark_new_rows = TRUE, new_rows_col_name = "someName")
  expect_equal(df_3$someName, c(0, 0, 1, 0, 1, 1, 0, 0, 0))
})

test_that("both wrapper functions, upsample() and downsample() work", {
  xpectr::set_test_seed(1)

  # Create data frame
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "score" = sample(c(1:100), 7)
  )

  xpectr::set_test_seed(1)
  df_min_balance <- balance(df, "min", "participant")
  xpectr::set_test_seed(1)
  df_min_downsample <- downsample(df, "participant")

  expect_equal(df_min_balance, df_min_downsample)

  xpectr::set_test_seed(1)
  df_max_balance <- balance(df, "max", "participant")
  xpectr::set_test_seed(1)
  df_max_upsample <- upsample(df, "participant")

  expect_equal(df_max_balance, df_max_upsample)
})

test_that("balance() works in dplyr pipeline", {
  xpectr::set_test_seed(1)

  xpectr::suppress_mw(library(dplyr))
  # Create data frame
  xpectr::set_test_seed(1)
  df <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 3, 3, 3)),
    "trial" = c(1, 2, 1, 1, 2, 3, 4),
    "score" = sample(c(1:100), 7)
  ) %>%
    balance("min", "participant")

  expect_equal(nrow(df), 3)
  expect_equal(df$participant, factor(c(1, 2, 3)))
  expect_equal(df$trial, c(2, 1, 3))
  expect_equal(ncol(df), 3)
})


test_that("balance() works with group_by()", {
  xpectr::set_test_seed(42)

  library(dplyr)

  ## Testing 'data.frame( "participant" = factor(c(1, 1, 2, ...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  xpectr::suppress_mw(output_19466 <- data.frame(
    "participant" = factor(c(1, 1, 2, 3, 2, 3, 3)),
    "trial" = c(1, 1, 1, 2, 2, 2, 4),
    "score" = sample(c(1:100), 7)
  ) %>%
    dplyr::group_by(trial) %>%
    balance("min", "participant")
  )
  # Testing class
  expect_equal(
    class(output_19466),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19466[["participant"]],
    structure(c(1L, 2L, 2L, 3L, 3L), .Label = c("1", "2", "3"), class = "factor"))
  expect_equal(
    output_19466[["trial"]],
    c(1, 1, 2, 2, 4),
    tolerance = 1e-4)
  expect_equal(
    output_19466[["score"]],
    c(92, 29, 62, 50, 70),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_19466),
    c("participant", "trial", "score"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19466),
    c("factor", "numeric", "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19466),
    c("integer", "double", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19466),
    c(5L, 3L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19466)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'data.frame( "participant" = factor(c(1, 1, 2, ...'     ####


})

